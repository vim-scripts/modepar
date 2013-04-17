" Vim plugin to load per-file Vim scripts.
" Maintainer:  Paul Isambert (zappathustra AT free DOT fr)
" Last Change: 2013 April 17

" Load once. {{{
if exists("g:loaded_modepar")
  finish
endif
let g:loaded_modepar = 1
" }}}

" s:getmodepars() {{{

" Patterns to find relevant lines; s:bpatt and s:vpatt are indentical except
" the latter is used to return the version number, if any; s:spatt finds
" one-line modepars.
let s:bpatt = '\(\_^\|\s\)modepar\([=<>]\?\d\+\|\d*\):'
let s:vpatt = '\(\_^\|\s\)modepar\zs\([=<>]\?\d\+\|\d*\)\ze:'
let s:spatt = '\(\_^\|\s\)modepar\([=<>]\?\d\+\|\d*\):[^:]*:\zs.\{-}\ze\s*\(:\s*:.*\)\?$'
let s:epatt = '\(\_^\|\s\)endmodepar'
let s:mpatt = '\s\(vim\?\|ex\)\([=<>]\?\d\+\|\d*\):'

" Gather all modepars in the file.
function! s:getmodepars ()
  " Modepars at the beginning of the file.
  let firstmodepars = []
  let m = s:getfirstmodepar(1)
  while len(m)
    call add(firstmodepars, m)
    let m = s:getfirstmodepar(m[1]+1)
  endwhile

  " Modepars at the end of the file.
  let lastmodepars = []
  " Ensures that the last modepar from the beginning of the file is not also
  " the first from the end of the file, i.e. that there is some non-modepar
  " material in the file; otherwise the same modepars would be loaded twice.
  if !len(firstmodepars) || firstmodepars[-1][1] < prevnonblank("$")
    let m = s:getlastmodepar(line("$"))
    while len(m)
      call insert(lastmodepars, m)
      let m = s:getlastmodepar(m[0]-1)
    endwhile
  endif

  return firstmodepars + lastmodepars
endfunction

" s:getfirstmodepar ({line}) {{{
" Get modepar from {line} downward.
function! s:getfirstmodepar (line)
  if a:line <= line("$")
    let l = nextnonblank(a:line)
    let L = getline(l)
    while match(L, s:mpatt) >= 0
      let l = nextnonblank(l+1)
      let L = getline(l)
    endwhile
    if match(L, s:bpatt) >= 0 " Line matches 'modepar...:'.
      if match(L, s:spatt) >=0
        let ll = l " One-line modepar.
      else
        let ll = l+1
        let LL = getline(ll)
        while match(LL, s:epatt) < 0
          if ll >= line("$")
            return [] " No 'endmodepar'.
          else
            let ll += 1
            let LL = getline(ll)
          endif
        endwhile
      endif
      return [l, ll]
    endif
  endif
  return []
endfunction
" }}}

" s:getlastmodepar ({line}) {{{
" Get modepar from {line} upward.
function! s:getlastmodepar (line)
  if a:line >= 1
    let ll = prevnonblank(a:line)
    let LL = getline(ll)
    while match(LL, s:mpatt) >= 0
      let ll = prevnonblank(ll-1)
      let LL = getline(ll)
    endwhile
    if match(LL, s:epatt) >= 0 " Line matches 'endmodepar'.
      let l = ll-1
      let L = getline(l)
      while match(L, s:bpatt) < 0
        if l <= 1
          return [] " No 'modepar' line.
        else
          let l -= 1
          let L = getline(l)
        endif
      endwhile
      return [l, ll]
    elseif match(LL, s:spatt) >= 0
      return [ll, ll] " One-line modepar.
    endif
  endif
  return []
endfunction
" }}}
" }}}

" s:domodepar({checkid}, {warn}}) {{{
function! s:domodepar (checkid, warn)
  " Return immediately if ids must be checked and none is registered.
  if a:checkid && (!exists("g:modepar_ids") || !len(g:modepar_ids))
    return
  endif

  let modepars = s:getmodepars()
  if len(modepars)
    while len(modepars)
      let [b, e] = remove(modepars, 0)
      let l = getline(b)

      " Check id.
      if a:checkid
        let id = substitute(matchstr(l, s:bpatt . '\s*\zs[^:]*'), '\s*$', '', '')
        let ok = 0
        for i in g:modepar_ids
          if id == i
            let ok = 1
            break
          endif
        endfor
        if !ok
          return
        endif
      endif

      " Check version.
      let vers = matchstr(l, s:vpatt)
      if len(vers)
        if match(vers, '^\d') >= 0
          if v:version < vers
            return
          endif
        else
          let [s, vers] = matchlist(vers, '\(.\)\(.*\)')[1:2]
          if (s == "=" && v:version != vers) || (s == "<" && v:version >= vers) || (s == ">" && v:version <= vers)
            return
          endif
        endif
      endif

      if !exists("s:tempfile")
        let s:tempfile = tempname()
      endif

      " Get whatever comes before 'endmodepar', removing trailing space.
      let p = matchstr(getline(e), '.*\zeendmodepar')
      let p = substitute(p, '\s*$', '', '')
      let pl = len(p)

      " Gather lines.
      let lines = []
      if b == e " One-line modepar.
        call add(lines, matchstr(getline(b), s:spatt))
      else
        let b += 1
        while b < e
          let l = getline(b)
          " Remove the material at the beginning of each line that was also
          " before 'endmodepar' (e.g. comments).
          if strpart(l, 0, pl) == p
            let l = strpart(l, pl)
          endif
          call add(lines, l)
          let b = b+1
        endwhile
      endif

      " Source script.
      call writefile(lines, s:tempfile)
      exe "so " . s:tempfile
    endwhile

  elseif a:warn
    echohl ErrorMsg
    echo "No modepar."
    echohl None
  endif
endfunction
" }}}

" s:switchmodepar({on/off}) {{{
function! s:switchmodepar(on)
  augroup modepar
    au!
    if a:on
      au  BufReadPost * call s:domodepar(1, 0)
    endif
  augroup END
endfunction
" }}}

" Commands. {{{
com! -bang Domodepar call s:domodepar(0, <bang>1)
com! -bang Modepar   call s:switchmodepar(<bang>1)
" }}}

" Enable modepars. {{{
if !exists("g:modepar_enable") || g:modepar_enable
  Modepar
endif
" }}}

" vim: foldmethod=marker
