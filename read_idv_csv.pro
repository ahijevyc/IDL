;+
; NAME:
;    READ_CSV
;
; PURPOSE:
;    Reads a comma-separated-variables (CSV) file into a structure.
;
; CATEGORY:
;    I/O
;
; CALLING SEQUENCE:
;    Result = READ_CSV(Filename)
;
; INPUTS:
;    Filename:  Name of CSV file. The file must contain a header line
;               of the form "# COL1,COL2,COL3...".
;
; KEYWORD PARAMETERS:
;    ULON64:    A comma-separated list of column names that are to be
;               treated as unsigned long 64-bit integers.
;
;    STRING:    A comma-separated list of column names that are to be
;               treated as strings.
;
;    DOUBLE:    A comma-separated list of column names that are to be
;               treated as doubles.
;
;    NULL:      If there are null fields in the file, they will be assigned
;               to the string specified by the NULL keyword before being
;               cast to the appropriate type.
;
; OUTPUTS:
;    The function returns a structure containing one field for each column
;    in the file, named by the entry in the file header. All fields are
;    assumed to be doubles unless their name contains the string 'OBJID',
;    in which case they are assumed to be unsigned long 64-bit integers,
;    or 'NAME', in which case they are assumed to be strings (this comparison
;    is case-insensitive). These defaults can all be over-ridden by using the
;    ULON64, STRING, and DOUBLE keywords.
;    The structure also contains the fields NROWS and NCOLS which contain
;    the number of rows and columns respectively.
;
; EXAMPLE:
;    If the file 'galaxies.csv' consists of:
;
;    # NAME,OBJID,RA,DEC
;    M31,493,10.68,41.27
;    NGC 1068,92,40.67,-0.01
;
;    Then reading it in produces:
;
;    IDL> Galstruct = READ_CSV('galaxies.csv')
;    IDL> HELP, /STRUCT, Galstruct
;    ** Structure <8237564>, 6 tags, length=80, data length=80, refs=1:
;    NAME            STRING    Array[2]
;    OBJID           ULONG64   Array[2]
;    RA              DOUBLE    Array[2]
;    DEC             DOUBLE    Array[2]
;    NROWS           LONG                 2
;    NCOLS           LONG                 4
;
;
; MODIFICATION HISTORY:
;    Modified by: Jeremy Bailin
;    12 August 2008  Use IDL 5.6 FILE_LINES to count number of lines.
;    Modified by: Vittorio Brando
;    12 August 2008 added function get_nlines to count lines in not unix OS
;                   added function valid_tag_name to ensure that the tag names are IDL compliant
;
;    Written by:   Jeremy Bailin
;    10 June 2008  Public release in JBIU
;    jbiu@astroconst.org
;
;-
;------------------------------------------------------------------------
function valid_tag_name,tmp_name
  ; ensure that the tag names are IDL compliant
  ; Tag names may not be IDL Reserved Words,
  ; and must be unique within a given structure
  ; Structure names and tag names follow the rules of IDL identifiers:
  ; they must begin with a letter; following characters can be letters,
  ; digits, or the underscore or dollar sign characters; and case is ignored.

  reserved_words=['AND','BEGIN','BREAK','CASE','COMMON','COMPILE_OPT', $
    'CONTINUE','DO','ELSE','END','ENDCASE','ENDELSE','ENDFOR', $
    'ENDIF','ENDREP','ENDSWITCH','ENDWHILE','EQ','FOR', $
    'FORWARD_FUNCTION','FUNCTION','GE','GOTO','GT','IF', $
    'INHERITS','LE','LT','MOD','NE','NOT','OF','ON_IOERROR', $
    'OR','PRO','REPEAT','SWITCH','THEN','UNTIL','WHILE','XOR']
  nonvalid_chars="[]() /|\,.<>!@#%^&*+=-"
  
  test_reserved = where(tmp_name eq reserved_words, test_result)
  if test_result ne 0 then  tmp_name+="_"  ;append underscore
  
  tmp_name=strjoin(STRSPLIT(tmp_name,nonvalid_chars,/extract),"_")
  
  return, tmp_name
end
;------------------------------------------------------------------------
function read_idv_csv, filename, ulon64=ul64str, string=strstr, double=dblstr, $
 
 ; started editing this, but gave up - Ahijevych aug 15, 2012 
end
