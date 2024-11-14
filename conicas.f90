module conicas
use rndgen_mod
use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
implicit none
integer(i4) :: seed = 294727492 
type(rndgen) :: generator

contains

subroutine elipse( a , b , L_x , L_y , area )
    real(kind=dp), intent(in) :: a , b , L_x , L_y
    real(kind=dp), intent(out) :: area
    real(kind=dp) x_r , y_r
    integer(kind=i4) i , num_trials/1000000/ , num_hits/0/
    call generator%init(seed)
    do i = 1 , num_trials
        x_r = generator%real( -0.5*L_x , 0.5*L_x )
        y_r = generator%real( -0.5*L_y , 0.5*L_y )
        if ( -b*sqrt(1. - (x_r/a)**2)<= y_r  .and. y_r <= b*sqrt(1. - (x_r/a)**2) ) then
            num_hits = num_hits + 1
        endif
    enddo
    area = real(num_hits,kind=dp)/real(num_trials,kind=dp) * L_x * L_y
end subroutine elipse

subroutine hiperbole( a , b , L_x , L_y , area  ) 
    real(kind=dp), intent(in) :: a , b , L_x , L_y
    real(kind=dp), intent(out) :: area
    real(kind=dp) x_r , y_r
    integer(kind=i4) i , num_trials/1000000/ , num_hits/0/
    call generator%init(seed)
    do i = 1 , num_trials
        x_r = generator%real( -0.5*L_x , 0.5*L_x )
        y_r = generator%real( -0.5*L_y , 0.5*L_y )
        if ( x_r <= a .or. a <= x_r ) then
            if ( -b*sqrt((x_r/a)**2 - 1.) <= y_r .and. y_r <= b*sqrt((x_r/a)**2 - 1.) ) then
                num_hits = num_hits + 1
            endif
        endif
    enddo
    area = real(num_hits,kind=dp)/real(num_trials,kind=dp) * L_x * L_y
end subroutine hiperbole

subroutine parabola( a )
    real, intent(in) :: a

end subroutine parabola

end module conicas