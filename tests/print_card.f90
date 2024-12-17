program print_card_test
    use fbingo

    implicit none

    type(card) :: x

    ! create example card
    x = gen_card()

    ! dab the free space
    x%dab(3, 3) = .true.

    call print_card(x, gameno = 1, cardno = 1, indwin = .true.)
end program print_card_test
