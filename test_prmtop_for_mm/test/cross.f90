module functions
  interface crossp
    function crossp_(v1, v2) result(vn)
    real*8 vn(3)
    real*8 :: v1(3), v2(3)
    vn(1) = v1(2) * v2(3) - v1(3) * v2(2)
    vn(2) = v1(3) * v2(1) - v1(1) * v2(3)
    vn(3) = v1(1) * v2(2) - v1(2) * v2(1)
    return
    endfunction
  endinterface
endmodule
