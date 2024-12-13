module fbingo 
        implicit none

        private
        public card, gen_card, print_card

        type :: card
                integer, dimension(5, 5) :: face
                logical, dimension(5, 5) :: dab
        end type card

contains
        function rand15() result(x)
                ! function for generating random number between 1 and 15
                ! used for selecting values in gen_card

                implicit none
                
                real :: rand_real
                integer :: x

                call random_number(rand_real)
                x = int(rand_real * 15) + 1
        end function rand15

        function gen_card() result(new_card)
                implicit none

                ! card result
                type(card) :: new_card, temp_card

                ! candidate integer for space being filled
                integer :: x

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
                                x = rand15()
                                if (any(temp_card%face(:, 1) == colb(x))) then
                                        cycle
                                else
                                        temp_card%face(i, 1) = colb(x)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                x = rand15()
                                if (any(temp_card%face(:, 2) == coli(x))) then
                                        cycle
                                else
                                        temp_card%face(i, 2) = coli(x)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                if (i == 3) then
                                        temp_card%face(i, 3) = 0
                                        exit
                                end if
                                x = rand15()
                                if (any(temp_card%face(:, 3) == coln(x))) then
                                        cycle
                                else
                                        temp_card%face(i, 3) = coln(x)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                x = rand15()
                                if (any(temp_card%face(:, 4) == colg(x))) then
                                        cycle
                                else
                                        temp_card%face(i, 4) = colg(x)
                                        exit
                                end if
                        end do
                        do while (.true.)
                                x = rand15()
                                if (any(temp_card%face(:, 5) == colo(x))) then
                                        cycle
                                else
                                        temp_card%face(i, 5) = colo(x)
                                        exit
                                end if
                        end do
                end do

                ! set all dabs to 0
                temp_card%dab(:, :) = .false.

                new_card = temp_card
        end function gen_card

        subroutine print_card(x, gameno, cardno)
                implicit none
                type(card), intent(in) :: x
                integer, intent(in) :: gameno, cardno

                print *, 'Game #', gameno
                print *, 'Card #', cardno
                print *, x%face(1, :)
                print *, x%face(2, :)
                print *, x%face(3, :)
                print *, x%face(4, :)
                print *, x%face(5, :)
        end subroutine print_card

end module fbingo
