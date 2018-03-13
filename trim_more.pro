function trim_more, words, leading_zero=leading_zero
  if ~keyword_set(leading_zero) then leading_zero = 0
  for i = 0, n_elements(words)-1 do begin
    word = words[i]
    last_char = strmid(word, strlen(word)-1)
    ; trim trailing decimal and trailing zeroes to right of decimal
    while last_char eq '.' || (last_char eq '0' && strpos(word,'.') ne -1) do begin
      word = strmid(word,0,strlen(word)-1)
      last_char = strmid(word, strlen(word)-1)
    endwhile
    ; trim leading zero
    if leading_zero then if strmid(word,0,2) eq '0.' then word=strmid(word,1)
    words[i] = word
  endfor
  return, words
end
