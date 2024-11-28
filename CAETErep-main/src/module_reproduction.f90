module reproduction
    implicit none
    private
    public :: repro

contains

    subroutine repro(nppa, height1, seed_mass, n_seed)!, npp_after) ! seed_bank, new_seed_bank)
        use global_par
        use productivity
    

        ! Declaração das variáveis de entrada
        real(r_8), intent(in) :: height1
        real(r_4), intent(in) :: nppa
        !real(r_8), intent(in) :: seed_bank  


        ! Declaração das variáveis de saída
        real(r_4), intent(out) :: n_seed 
        !real(r_4), intent(out) :: npp_after 
        real(r_8), intent(out) :: seed_mass
        !real(r_8), intent(out) :: seed_bank
        !real(r_8), intent(out) :: new_seed_bank

        ! Variáveis internas
        real(r_8) :: height
        real(r_4) :: npp_rep
        real(r_8) :: seed_mass_log
        !real(r_8) :: seed_mass_one

        ! Calculando a massa da semente
        height = height1  ! Altura da planta em metros
        
        !print *, "valor de npp_rep:", npp_rep
        !print *, "valor de nppa", nppa

        ! Nova fórmula para a massa da semente em miligramas (mg)
        !if (height1 .gt. 5.0) then

        if (height .gt. 5.0) then
            seed_mass_log = (0.059 * height) + 0.8812
            !print *, "seed_mass_log", seed_mass_log
            ! Converte a massa da semente de escala logarítmica para normal (linear)
            seed_mass = ((10.0 ** seed_mass_log) / 1.0D6)
            !print *, "Altura da planta:", height, "m, Massa da semente:", seed_mass, "kg"

            npp_rep = nppa * 0.04  ! 4% do NPP disponível para reprodução
             ! Calculando o número de sementes produzidas
            n_seed = nint(npp_rep / seed_mass)

            !print *, "---------- n_seed:----------", n_seed
            !npp_after = nppa - npp_rep !remaining npp
        else
            n_seed = 0.0D0
            seed_mass_log = 0.0D0
            seed_mass = 0.0D0
            npp_rep = 0.0D0
            
            !print *, "*******PLS não tem altura suficiente para produzir sementes *******"
        endif

       

        ! Verificando se a massa da semente é menor que o limite de 8 mg
        !if (seed_mass < 8.0) then
        !    seed_mass = 8.0
        !endif

        
        !n_seed = int(n_seed)  ! Garantindo que o número de sementes seja um valor inteiro

        ! Garantindo que o número de sementes seja um valor inteiro
        ! Garantir que n_seed seja pelo menos 1 se houver produção
        !if (n_seed < 1 .and. npp_rep > 0) then
        !    n_seed = 1
        !endif
        
        !print*, "numero de sementes produzidas:", n_seed
        !print *, "Tamanho do banco de sementes antes da produção:", seed_bank

        !print *, "new_seed_bank (antes da atualização)_module_reproduction:", new_seed_bank
        !print *, "seed_bank(antes da atualização)_module_reproduction:", new_seed_bank
      

        ! Atualização do banco de sementes
        !if (n_seed .gt. 0) then
        !    new_seed_bank = seed_bank + n_seed
        !else
        !    new_seed_bank = seed_bank  ! Não altera se não houver produção
        !endif


        ! Atualizando o banco de sementes
        !new_seed_bank = seed_bank + n_seed
        !seed_bank = new_seed_bank

        ! Evitar que seed_bank assuma valores não realistas
        !if (seed_bank < 0) then
        !    seed_bank = 0
        !endif

        ! Imprime para depuração
        !print *, "Tamanho do banco de sementes após a produção_module_repro:", new_seed_bank

    end subroutine repro

end module reproduction
