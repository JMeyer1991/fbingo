module cards
        implicit none

        private
        public gen_card

contains
        function gen_card() result(card)
                ! card result
                integer, dimension(5, 5) :: card, temp_card

                ! randomizer values
                integer :: rand_int
                real :: rand_real

                ! row index
                integer :: i

                ! variables specifying values for each column
                integer, dimension(15) :: colb, coli, coln, colg, colo

                ! set possible values for each column
                colb = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
                coli = [16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, &
                        29, 30]
                coln = [31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, &
                        44, 45]
                colg = [46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, &
                        59, 60]
                colo = [61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, &
                        74, 75]

                do i = 1, 5
                        do while (.true.)
                                call random_number(rand_real)
                                rand_int = int(rand_real * 15) + 1
                                if (any(temp_card(:, 1) == colb(rand_int))) then
                                        cycle
                                else
                                        temp_card(i, 1) = colb(rand_int)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                call random_number(rand_real)
                                rand_int = int(rand_real * 15) + 1
                                if (any(temp_card(:, 2) == coli(rand_int))) then
                                        cycle
                                else
                                        temp_card(i, 2) = coli(rand_int)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                if (i == 3) then
                                        temp_card(i, 3) = 0
                                        exit
                                end if
                                call random_number(rand_real)
                                rand_int = int(rand_real * 15) + 1
                                if (any(temp_card(:, 3) == coln(rand_int))) then
                                        cycle
                                else
                                        temp_card(i, 3) = coln(rand_int)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                call random_number(rand_real)
                                rand_int = int(rand_real * 15) + 1
                                if (any(temp_card(:, 4) == colg(rand_int))) then
                                        cycle
                                else
                                        temp_card(i, 4) = colg(rand_int)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                call random_number(rand_real)
                                rand_int = int(rand_real * 15) + 1
                                if (any(temp_card(:, 5) == colo(rand_int))) then
                                        cycle
                                else
                                        temp_card(i, 5) = colo(rand_int)
                                        exit
                                end if
                        end do
                end do

                card(:, :) = temp_card(:, :)
        end function gen_card

end module cards
