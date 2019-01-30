subroutine date_and_tim (cdate, ctime)
    !
    implicit none
    character (len=9) :: cdate, ctime
    !
    character(len=3), dimension(12) :: months
    data months /'Jan','Feb','Mar','Apr','May','Jun', 'Jul','Aug','Sep','Oct','Nov','Dec'/
    INTEGER date_time(8)
    !
    call date_and_time(values=date_time)
    !
    write (cdate,'(i2,a3,i4)') date_time(3),months(date_time(2)),date_time(1)
    write (ctime,'(i2,":",i2,":",i2)') date_time(5), date_time(6), date_time(7)

end subroutine date_and_tim
