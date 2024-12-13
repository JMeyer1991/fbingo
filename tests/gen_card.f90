program gen_card_test
        use fbingo
        implicit none

        integer, dimension(5, 5) :: card

        card = gen_card()

        print *, 'B:', card(:, 1)
        print *, 'I:', card(:, 2)
        print *, 'N:', card(:, 3)
        print *, 'G:', card(:, 4)
        print *, 'O:', card(:, 5)
end program gen_card_test
