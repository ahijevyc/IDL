pro iter
n = 60
f = 2
b = 1

if f-b le 0 then stop
iter = 0
nf = 0
while 1 do begin
  increment = iter mod (f+b) lt f ? 1 : -1
  nf = nf + increment
  iter=iter+1
  print, iter, nf
  if nf eq n then break
endwhile
print, (n-f)*(f+b)/(f-b)+f
end