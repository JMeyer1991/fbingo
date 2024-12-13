program gen_card_test
        use fbingo
        implicit none

        type(card) :: x

        x = gen_card()

        print *, 'B:', x%face(:, 1)
        print *, 'B:', x%dab(:, 1)
        print *, 'I:', x%face(:, 2)
        print *, 'I:', x%dab(:, 2)
        print *, 'N:', x%face(:, 3)
        print *, 'N:', x%dab(:, 3)
        print *, 'G:', x%face(:, 4)
        print *, 'G:', x%dab(:, 4)
        print *, 'O:', x%face(:, 5)
        print *, 'O:', x%dab(:, 5)
end program gen_card_test
