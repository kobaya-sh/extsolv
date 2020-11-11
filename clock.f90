subroutine clock_set(tag)
integer lnow
real*8 torg(8)
character(len=32*8) path
character(len=*) tag
integer ppath(8)
common/wallclock/torg, lnow, path, ppath
lnow = 2
call cpu_time(torg(1))
torg(2) = torg(1)
path = tag // '/'
ppath(lnow) = len(tag) + 1
endsubroutine

subroutine clock_clear
integer lnow
real*8 torg(8)
character(len=32*8) path
integer ppath(8)
common/wallclock/torg, lnow, path, ppath
ppath = 0
lnow = 1
call clock_look('@Clock_Clear')
lnow = 0
endsubroutine

subroutine clock_push(tag)
integer lnow
real*8 torg(8)
character(len=*) tag
character(len=32*8) path
integer ppath(8)
common/wallclock/torg, lnow, path, ppath
if (lnow >= 7) then
  write(*, *) 'clock_push: stack overflows.'
  stop
endif
path = path(1:ppath(lnow)) // tag // '/'
ppath(lnow + 1) = ppath(lnow) + len(tag) + 1
lnow = lnow + 1
call cpu_time(torg(lnow))
!call clock_look('@Start')
endsubroutine

subroutine clock_pop()
integer lnow
real*8 torg(8)
character(len=32*8) path
integer ppath(8)
common/wallclock/torg, lnow, path, ppath
!path(ppath(lnow):ppath(lnow)+3) = "@pop"
call clock_look('@End')
lnow = lnow - 1
if (lnow <= 0) then
  write(*, *) 'clock_pop: stack is empty.'
  stop
endif
endsubroutine

subroutine clock_look(tag)
integer lnow
real*8 torg(8), tnow
character(len=*) tag
character(len=32*8) path
integer ppath(8)
common/wallclock/torg, lnow, path, ppath
call cpu_time(tnow)
write(*, '("lap: ", f8.3, "  clock: "$)') tnow - torg(lnow)
if (lnow > 1) then
  write(*, '(f8.3$)') tnow - torg(1:lnow-1)
endif
write(*, '("  (",a,")")') path(1:ppath(lnow)) // tag
torg(lnow) = tnow
endsubroutine
