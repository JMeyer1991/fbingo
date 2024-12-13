program bingo
        use cards
        ! loop indices
        integer :: i, j, k

        ! number of cards/games to simulate
        integer :: ncards, ngames

        ! integer and logical card faces
        integer, allocatable, dimension(:, :, :, :) :: icards
        logical, allocatable, dimension(:, :, :, :) :: lcards

        ! get simulation parameters from user
        print *, 'How many bingo cards to simulate?'
        read(*, *) ncards
        print *, 'How many games to simulate?'
        read(*, *) ngames

        ! allocate card faces
        allocate(icards(5, 5, ncards, ngames))
        allocate(lcards(5, 5, ncards, ngames))

        ! for each game
        do i = 1, ngames
                ! for each card
                do j = 1, ncards
                        icards(:, :, j, i) = gen_card()
                        print *, icards(:, :, :, :) 
                end do
        end do
end program bingo
