function get_structure_tag, struct, field
  tnames = TAG_NAMES(struct)
  tindex = where(strcmp(tnames,field,/fold) eq 1, n)
  if n gt 0 then return, struct.(tindex) else message, 'no '+field+' found in structure'
end