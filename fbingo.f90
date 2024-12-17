module fbingo 
    implicit none

    private
    public card, gen_card, print_card

    integer, dimension(15, 5) :: balls

    type :: card
    logical, dimension(5, 5) :: dab
    integer, dimension(5, 5) :: face
    logical :: winner
    end type card

    data balls / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, &
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, &
        31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, &
        46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, &
        61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75 /

contains

    function gen_card() result(new_card)
        implicit none

        ! card result
        type(card) :: new_card, temp_card

        ! candidate integer for space being filled
        integer :: x

        ! loop indices
        integer :: i, j

        ! assign numbers to card face
        do i = 1, 5
            do j = 1, 5
                do while (.true.)
                    x = balls(rand15(), j)
                        if (any(temp_card%face(:, j) == x)) then
                            cycle
                        else
                            temp_card%face(i, j) = x 
                            exit
                        end if
                end do
            end do
        end do

        ! set free space to 0
        temp_card%face(3, 3) = 0

        ! set all dabs to 0
        temp_card%dab(:, :) = .false.

        ! card is not yet a winner
        temp_card%winner = .false.

        ! return generated card value
        new_card = temp_card
    end function gen_card

    subroutine print_card(x, gameno, cardno, indwin)
        implicit none

        integer, optional, intent(in) :: cardno
        integer :: i
        logical, optional, intent(in) :: indwin
        integer :: j
        integer, optional, intent(in) :: gameno
        type(card), intent(in) :: x
        character(len=3), dimension(5, 5) :: y

        ! if indwin is present and true, display whether card is winner
        if(present(indwin)) then
            if (indwin .and. x%winner) then
                print *, 'WINNER!'
            else if (indwin .and. x%winner .eqv. .false.) then
                print *, 'NOT A WINNER'
            end if
        end if

        ! if gameno is present, diplsay the game number
        if (present(gameno)) then
            print *, 'Game #', gameno
        end if

        ! if cardno is present, display the card number
        if (present(cardno)) then
            print *, 'Card #', cardno
        end if

        ! convert card face to character vector
        do i = 1, 5
            do j = 1, 5
                if (x%dab(i, j)) then
                    y(i, j) = ' * ' 
                else
                    write(y(i, j), '(i2, 1x)') x%face(i, j) 
                end if
            end do
        end do

        print *, y(1, :)
        print *, y(2, :)
        print *, y(3, :)
        print *, y(4, :)
        print *, y(5, :)
    end subroutine print_card

    function rand15() result(x)
        ! function for generating random number between 1 and 15
        ! used for selecting values in gen_card

        implicit none
                
        real :: rand_real
        integer :: x

        call random_number(rand_real)
        x = int(rand_real * 15) + 1
    end function rand15

end module fbingo
