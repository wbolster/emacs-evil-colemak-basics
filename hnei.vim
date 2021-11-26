" The main magic is done by the following langmap setting.  Not only does it
" make Colemak's "hnei" work like QWERTY's "hjkl", but it also covers mappings
" like "gj", "zj", etc.
"
" THE ONLY CAVEAT IS THAT MOST MACROS WILL BE BROKEN!  Until this bug is fixed
" in Vim, it is recommended to record a macro with an empty langmap.
"
" The t-f-j rotation is enabled by default but can be disabled using:
"let g:colemak_basics_rotate_t_f_j = 0
if get(g:, 'colemak_basics_rotate_t_f_j', 1)
  set langmap=nN;jJ,eE;kK,iI;lL,kK;nN,uU;iI,lL;uU,fF;eE,tT;fF,jJ;tT
else
  set langmap=nN;jJ,eE;kK,iI;lL,kK;nN,uU;iI,lL;uU,jJ;eE
endif

" Do not apply the langmap to characters resulting from a mapping.
set nolangremap

" Now, we only need to add few remappings not covered by langmap.  Note that
" the langmap setting does not apply to "CTRL + <KEY>", but fortunately, these
" mappings either have semantics different from those without "CTRL" or may be
" disregarded as there are better alternatives covered by langmap (like
" "CTRL-W CTRL-J", which is mapped to the same function as "CTRL-W j").

" Disable timing out on mappings as it makes mapped key sequences unreliable:
" any intermediate delay of more than 1 second would break them.
set notimeout

" Insert mode remappings.  See ":help i_CTRL-G_j" and ":help i_CTRL-G_k".
inoremap <c-g>n <c-g>j
inoremap <c-g><c-n> <c-g><c-j>
inoremap <c-g>e <c-g>k
inoremap <c-g><c-e> <c-g><c-k>
" Due to some bug in Vim 8.0, the following two tautological lines are also
" necessary for the Insert mode remappings to work.
inoremap <c-g>j <c-g>j
inoremap <c-g>k <c-g>k
