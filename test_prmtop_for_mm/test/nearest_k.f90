program main
call nearest_k(k)
write(*, *) k
endprogram

subroutine nearest_k(k)
integer, parameter :: mask = z"3f"
integer q(4, mask)
integer s, t
integer i
i = 1
s = 0
t = 1
q(:, s) = (/0, 0, 0, 0/)
do
  if (q(4, s) > k) then
    k = q(4, s)
    return
  endif
  q(:, iand(mask, t+1)) = (/q(1, s) + 2, q(2, s) + 1, q(3, s), q(4, s)/)
  q(:, iand(mask, t+2)) = (/q(1, s) + 3, q(2, s), q(3, s) + 1, q(4, s)/)
  q(:, iand(mask, t+3)) = (/q(1, s) + 5, q(2, s), q(3, s), q(4, s) + 1/)
  t = iand(mask, t+3)
  s = iand(mask, s+1)



