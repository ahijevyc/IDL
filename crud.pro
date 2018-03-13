pro crud
  theta = findgen(200)*10
  k = .09
  r = theta^k
  plot, /polar, r, theta, /iso
  
end