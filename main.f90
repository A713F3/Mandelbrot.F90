program raycasting
implicit none
    integer :: width = 1000, height = 1000, iteration, max_iteration
    real :: scaled_x, scaled_y, x, y, x_temp
    integer :: px, py, color

    open(1, file="output.ppm")

    write(1, "(A2)") "P3"
    write(1, "(I5,A1,I5)") width, " ", height
    write(1, "(I3)") 255

    do py=1, height
        do px=1, width
            scaled_x = map_val(real(px), 1.0, real(width), -2.00, 0.47)
            scaled_y = map_val(real(py), 1.0, real(height), -1.12, 1.12)

            x = 0.0
            y = 0.0

            iteration = 0
            max_iteration = 1000

            do while(x*x + y*y .le. 2*2 .and. iteration .lt. max_iteration)
                x_temp = x*x - y*y + scaled_x
                y = 2*x*y + scaled_y
                x = x_temp
                iteration = iteration + 1
            end do

            color = 255 - int(iteration * (255.0/max_iteration))

            write(1, "(I3, X, I3, X, I3, X)", advance="no") color, color, color
        end do
    end do

    close(1)

contains 
    function map_val(val, a1, a2, b1, b2)
        real :: val, a1, a2, b1, b2, map_val

        map_val = b1 + (((val - a1)*(b2 - b1)) / (a2 - a1))
    end function

end program