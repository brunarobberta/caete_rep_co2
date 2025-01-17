
! Copyright 2017- LabTerra

!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.

!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.

!     You should have received a copy of the GNU General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.

! contacts :: David Montenegro Lapola <lapoladm ( at ) gmail.com>
! Author: JP Darela
! This program is based on the work of those that gave us the INPE-CPTEC-PVM2 model

module budget
   implicit none
   private

   public :: daily_budget

contains

   subroutine daily_budget(dt, w1, w2, ts, temp, prec, n_days, stored_seed_bank, p0, ipar, rh&
        &, mineral_n, labile_p, on, sop, op, csoil,catm, sto_budg_in, cl1_in, ca1_in, cf1_in, nleaf_in, nwood_in&
        &, nroot_in, uptk_costs_in, wmax_in, evavg, epavg, phavg, aravg, nppavg&
        &, laiavg, rcavg, f5avg, rmavg, rgavg, cleafavg_pft, cawoodavg_pft&
        &, cfrootavg_pft, storage_out_bdgt_1, ocpavg, wueavg, cueavg, c_defavg&
        &, vcmax_1, specific_la_1, nupt_1, pupt_1, litter_l_1, cwd_1, litter_fr_1, npp2pay_1, lit_nut_content_1&
        &, delta_cveg_1, seed_bank_out_bdgt, co2_abs_se_1, limitation_status_1, uptk_strat_1, cp, c_cost_cwm&
        &, height_aux, seed_mass_out_budg)


      use types
      use global_par, only: ntraits, npls
      use alloc
      use productivity
      use omp_lib

      use reproduction  !NEW ***************

      use photo
      use water, only: evpot2, penman, available_energy, runoff

!     ----------------------------INPUTS-------------------------------
      real(r_8),dimension(ntraits,npls),intent(in) :: dt
      real(r_8),intent(in) :: w1, w2   !Initial (previous month last day) soil moisture storage (mm)
      ! real(r_4),dimension(npls),intent(in) :: g1   !Initial soil ice storage (mm)
      ! real(r_4),dimension(npls),intent(in) :: s1   !Initial overland snow storage (mm)
      real(r_4),intent(in) :: ts                   ! Soil temperature (oC)
      real(r_4),intent(in) :: temp                 ! Surface air temperature (oC)

      real(r_4),intent(in) :: prec                   ! Precipitation (mm/day) !NEW ***************
      real(r_4),intent(in) :: n_days                 ! Day of year !NEW ***************

      real(r_4),intent(in) :: p0                   ! Surface pressure (mb)
      real(r_4),intent(in) :: ipar                 ! Incident photosynthetic active radiation mol Photons m-2 s-1
      real(r_4),intent(in) :: rh                   ! Relative humidity
      real(r_4),intent(in) :: mineral_n            ! Solution N NOx/NaOH gm-2
      real(r_4),intent(in) :: labile_p             ! solution P O4P  gm-2
      real(r_8),intent(in) :: on, sop, op          ! Organic N, isoluble inorganic P, Organic P g m-2
      real(r_8),intent(in) :: catm, wmax_in                 ! ATM CO2 concentration ppm
      real(r_8),intent(in) :: csoil                !Soil Organic Carbon', 'g m-2'


      real(r_8),dimension(3,npls),intent(in)  :: sto_budg_in ! Rapid Storage Pool (C,N,P)  g m-2
      real(r_8),dimension(npls),intent(in) :: cl1_in  ! initial BIOMASS cleaf compartment kgm-2
      real(r_8),dimension(npls),intent(in) :: cf1_in  !                 froot
      real(r_8),dimension(npls),intent(in) :: ca1_in  !                 cawood
      real(r_8),dimension(npls),intent(in) :: nleaf_in  ! CHANGE IN cVEG (DAILY BASIS) TO GROWTH RESP
      real(r_8),dimension(npls),intent(in) :: nroot_in  ! k gm-2
      real(r_8),dimension(npls),intent(in) :: nwood_in  ! k gm-2
      real(r_8),dimension(npls),intent(in) :: uptk_costs_in ! g m-2
      real(r_8),dimension(npls),intent(in) :: stored_seed_bank  ! NEW (module_reproduction)



      !     ----------------------------OUTPUTS------------------------------
      real(r_4),intent(out) :: epavg          !Maximum evapotranspiration (mm/day)
      real(r_8),intent(out) :: evavg          !Actual evapotranspiration Daily average (mm/day)
      real(r_8),intent(out) :: phavg          !Daily photosynthesis (Kg m-2 y-1)
      real(r_8),intent(out) :: aravg          !Daily autotrophic respiration (Kg m-2 y-1)
      real(r_8),intent(out) :: nppavg         !Daily NPP (average between PFTs)(Kg m-2 y-1)
      real(r_8),intent(out) :: laiavg         !Daily leaf19010101', '19551231 area Index m2m-2
      real(r_8),intent(out) :: rcavg          !Daily canopy resistence s/m
      real(r_8),intent(out) :: f5avg          !Daily canopy resistence s/m
      real(r_8),intent(out) :: rmavg          !maintenance/growth respiration (Kg m-2 y-1)
      real(r_8),intent(out) :: rgavg          !maintenance/growth respiration (Kg m-2 y-1)
      real(r_8),intent(out) :: wueavg         ! Water use efficiency
      real(r_8),intent(out) :: cueavg         ! [0-1]
      real(r_8),intent(out) :: vcmax_1          ! µmol m-2 s-1
      real(r_8),intent(out) :: specific_la_1    ! m2 g(C)-1
      real(r_8),intent(out) :: c_defavg       ! kg(C) m-2 Carbon deficit due to negative NPP - i.e. ph < ar
      real(r_8),intent(out) :: litter_l_1       ! g m-2
      real(r_8),intent(out) :: cwd_1            ! g m-2
      real(r_8),intent(out) :: litter_fr_1      ! g m-2
      real(r_8),intent(out) :: co2_abs_se_1
      real(r_8),dimension(2),intent(out) :: nupt_1         ! g m-2 (1) from Soluble (2) from organic
      real(r_8),dimension(3),intent(out) :: pupt_1         ! g m-2
      real(r_8),dimension(6),intent(out) :: lit_nut_content_1 ! g(Nutrient)m-2 ! Lit_nut_content variables         [(lln),(rln),(cwdn),(llp),(rl),(cwdp)]

      ! FULL OUTPUT
      real(r_8),dimension(npls),intent(out) :: cleafavg_pft   !Carbon in plant tissues (kg m-2)
      real(r_8),dimension(npls),intent(out) :: cawoodavg_pft  !
      real(r_8),dimension(npls),intent(out) :: cfrootavg_pft  !
      real(r_8),dimension(npls),intent(out) :: ocpavg         ! [0-1] Gridcell occupation
      real(r_8),dimension(3,npls),intent(out) :: delta_cveg_1
      real(r_8),dimension(3,npls),intent(out) :: storage_out_bdgt_1
      integer(i_2),dimension(3,npls),intent(out) :: limitation_status_1
      integer(i_4),dimension(2,npls),intent(out) :: uptk_strat_1
      real(r_8),dimension(npls),intent(out) ::  npp2pay_1 ! C costs of N/P uptake
      real(r_8),dimension(4),intent(out) :: cp ! Aux cp(1:3) CVEG C POOLS cp(4) Auxiliary to HR
      real(r_8),intent(out) :: c_cost_cwm
      real(r_8),dimension(npls),intent(out) ::  seed_bank_out_bdgt, height_aux, seed_mass_out_budg !! NEW (module_reproduction)

      !     -----------------------Internal Variables------------------------
      integer(i_4) :: p, counter, nlen, ri, i, j
      real(r_8),dimension(ntraits) :: dt1 ! Store one PLS attributes array (1D)
      real(r_8) :: carbon_in_storage
      real(r_8) :: testcdef
      real(r_8) :: sr, mr_sto, growth_stoc
      real(r_8),dimension(npls) :: ocp_mm      ! TODO include cabon of dead plssss in the cicle?
      logical(l_1),dimension(npls) :: ocp_wood
      integer(i_4),dimension(npls) :: run

      real(r_4),parameter :: tsnow = -1.0
      real(r_4),parameter :: tice  = -2.5

      real(r_8),dimension(npls) :: cl1_pft, cf1_pft, ca1_pft!, seed_bank_pft !new seed_bank
      real(r_4) :: soil_temp
      real(r_4) :: emax
      real(r_8) :: w                               !Daily soil moisture storage (mm)

      real(r_8),dimension(:),allocatable :: ocp_coeffs

      real(r_4),dimension(:),allocatable :: evap   !Actual evapotranspiration (mm/day)
      !c     Carbon Cycle
      real(r_4),dimension(:),allocatable :: ph     !Canopy gross photosynthesis (kgC/m2/yr)
      real(r_4),dimension(:),allocatable :: ar     !Autotrophic respiration (kgC/m2/yr)
      real(r_4),dimension(:),allocatable :: nppa   !Net primary productivity / auxiliar
      real(r_8),dimension(:),allocatable :: laia   !Leaf area index (m2 leaf/m2 area)
      real(r_4),dimension(:),allocatable :: rc2    !Canopy resistence (s/m)
      real(r_4),dimension(:),allocatable :: f1     !
      real(r_8),dimension(:),allocatable :: f5     !Photosynthesis (mol/m2/s)
      real(r_4),dimension(:),allocatable :: vpd    !Vapor Pressure deficit
      real(r_4),dimension(:),allocatable :: rm     !maintenance & growth a.resp
      real(r_4),dimension(:),allocatable :: rg
      real(r_4),dimension(:),allocatable :: wue
      real(r_4),dimension(:),allocatable :: cue
      real(r_4),dimension(:),allocatable :: c_def

      real(r_4),dimension(:),allocatable :: max_nppa    !! NEW (module_reproduction)
      real(r_4),dimension(:),allocatable :: acumulated_biomass    !! NEW (module_reproduction)
      real(r_8),dimension(:),allocatable :: seed_mass    !! NEW (module_reproduction)
      real(r_8),dimension(:),allocatable :: seed_bank_int_repro   !! NEW (module_reproduction)
      real(r_8),dimension(:),allocatable :: seed_bank_out_repro   !! NEW (module_reproduction)
      real(r_4),dimension(:),allocatable :: decayed_seed_bank  !! NEW (module_reproduction)
      real(r_4),dimension(:),allocatable :: n_seed   !! NEW (module_reproduction)
      real(r_4),dimension(:),allocatable :: germinated_seeds  !! NEW (module_reproduction)
      real(r_8),dimension(12) :: rain_last_days
      integer(r_8) :: i_day
      real(r_8) :: rain_last_10_days
      real(r_8) :: rain_accum_last_10_days

      real(r_8),dimension(:),allocatable :: cl1_int
      real(r_8),dimension(:),allocatable :: cf1_int
      real(r_8),dimension(:),allocatable :: ca1_int
      real(r_8),dimension(:),allocatable :: tra
      real(r_8),dimension(:),allocatable :: cl2
      real(r_8),dimension(:),allocatable :: cf2
      real(r_8),dimension(:),allocatable :: ca2    ! carbon pos-allocation
      real(r_8),dimension(:,:),allocatable :: day_storage      ! D0=3 g m-2
      real(r_8),dimension(:),allocatable   :: vcmax            ! µmol m-2 s-1
      real(r_8),dimension(:),allocatable   :: specific_la      ! m2 g(C)-1
      real(r_8),dimension(:,:),allocatable :: nupt             !d0 =2      ! g m-2 (1) from Soluble (2) from organic
      real(r_8),dimension(:,:),allocatable :: pupt             !d0 =3      ! g m-2
      real(r_8),dimension(:),allocatable   :: litter_l         ! g m-2
      real(r_8),dimension(:),allocatable   :: cwd              ! g m-2
      real(r_8),dimension(:),allocatable   :: litter_fr        ! g m-2
      real(r_8),dimension(:),allocatable   :: npp2pay          ! G M-2
      real(r_8),dimension(:,:),allocatable :: lit_nut_content  ! d0=6 g(Nutrient)m-2 ! Lit_nut_content variables         [(lln),(rln),(cwdn),(llp),(rl),(cwdp)]
      real(r_8),dimension(:,:),allocatable :: delta_cveg       ! d0 = 3
      real(r_8),dimension(:,:),allocatable :: storage_out_bdgt ! d0 = 3
      real(r_8),dimension(:),allocatable :: ar_fix_hr ! store plss npp (C) exchanged with N fixer µorganisms GOTO HR
      integer(i_2),dimension(:,:),allocatable   :: limitation_status ! D0=3
      integer(i_4), dimension(:, :),allocatable :: uptk_strat        ! D0=2
      INTEGER(i_4), dimension(:), allocatable :: lp ! index of living PLSs/living grasses
      real(r_8),dimension(:), allocatable :: height_int
      real(r_8),dimension(:), allocatable :: crown_int
      real(r_8),dimension(:), allocatable :: co2_abs_se
      ! real(r_8),dimension(:), allocatable :: fpc_grid_int

      real(r_8), dimension(npls) :: awood_aux, nleaf, nwood, nroot, uptk_costs, pdia_aux, dwood_aux, sla_aux
      real(r_8), dimension(3,npls) :: sto_budg
      real(r_8) :: soil_sat, ar_aux
      real(r_8), dimension(:), allocatable :: idx_grasses, idx_pdia
      real(r_8), dimension(npls) :: diameter_aux, crown_aux
      real(r_8), dimension(npls) :: delta_biomass
      real(r_8) :: max_height



      !     START
      !     --------------
      !     Grid cell area fraction 0-1
      !     ============================

      ! create copies of some input variables (arrays) - ( they are passed by reference by standard)
      do i = 1,npls
         awood_aux(i) = dt(7,i)
         pdia_aux(i) = dt(17,i)
         dwood_aux(i) = dt(18,i)
         sla_aux(i) = dt(19,i)
         cl1_pft(i) = cl1_in(i)
         ca1_pft(i) = ca1_in(i)
         cf1_pft(i) = cf1_in(i)
         nleaf(i) = nleaf_in(i)
         nwood(i) = nwood_in(i)
         nroot(i) = nroot_in(i)
         uptk_costs(i) = uptk_costs_in(i)
         seed_mass_out_budg(i) = 0.0D0



         do j = 1,3
            sto_budg(j,i) = sto_budg_in(j,i)
         enddo

         !print *, "cl1_pft(in)", cl1_pft(i)

      enddo



      ! find the number of grasses

      w = w1 + w2      ! soil water mm
      soil_temp = ts   ! soil temp °C
      soil_sat = wmax_in

      call pft_area_frac(cl1_pft, cf1_pft, ca1_pft, awood_aux,&
      &                  ocpavg, ocp_wood, run, ocp_mm)

      call pls_allometry(dt, ca1_pft,awood_aux, height_aux, diameter_aux,&
      &                   crown_aux)


      ! print*, 'CAWOOD (kg/m2)', ca1_pft
      ! print*, 'DIAMETER', diameter_aux

      max_height = maxval(height_aux(:))


      nlen = sum(run)    ! New length for the arrays in the main loop

      allocate(lp(nlen))
      allocate(ocp_coeffs(nlen))
      allocate(idx_grasses(nlen))
      allocate(idx_pdia(nlen))
      allocate(ar_fix_hr(nlen))

      idx_grasses(:) = 1.0D0
      idx_pdia(:) = 1.0D0

      ! Get only living PLSs
      counter = 1
      do p = 1,npls
         if(run(p).eq. 1) then
            lp(counter) = p
            ocp_coeffs(counter) = ocpavg(p)
            counter = counter + 1
         endif
      enddo

      ! Identify grasses
      do p = 1, nlen
         if (awood_aux(lp(p)) .le. 0.0D0) idx_grasses(p) = 0.0D0
      enddo
      ! Identify Nfixers
      do p = 1, nlen
         if (pdia_aux(lp(p)) .le. 0.0D0) idx_pdia(p) = 0.0D0
      enddo

      allocate(evap(nlen))
      allocate(nppa(nlen))
      allocate(ph(nlen))
      allocate(ar(nlen))
      allocate(laia(nlen))
      allocate(f5(nlen))
      allocate(f1(nlen))
      allocate(vpd(nlen))
      allocate(rc2(nlen))
      allocate(rm(nlen))
      allocate(rg(nlen))
      allocate(wue(nlen))
      allocate(cue(nlen))
      allocate(c_def(nlen))
      allocate(vcmax(nlen))
      allocate(specific_la(nlen))
      allocate(storage_out_bdgt(3, nlen))
      allocate(tra(nlen))
      allocate(nupt(2, nlen))
      allocate(pupt(3, nlen))
      allocate(litter_l(nlen))
      allocate(cwd(nlen))
      allocate(litter_fr(nlen))
      allocate(lit_nut_content(6, nlen))
      allocate(delta_cveg(3, nlen))
      allocate(npp2pay(nlen))
      allocate(limitation_status(3,nlen))
      allocate(uptk_strat(2,nlen))
      allocate(cl1_int(nlen))
      allocate(cf1_int(nlen))
      allocate(ca1_int(nlen))
      allocate(cl2(nlen))
      allocate(cf2(nlen))
      allocate(ca2(nlen))
      allocate(day_storage(3,nlen))
      allocate(height_int(nlen))
      allocate(crown_int(nlen))
      allocate(co2_abs_se(nlen))

      allocate(seed_mass(nlen)) !! NEW (module_reproduction)
      allocate(seed_bank_int_repro(nlen)) !! NEW (module_reproduction)
      allocate(seed_bank_out_repro(nlen)) !! NEW (module_reproduction)
      allocate(decayed_seed_bank(nlen)) !! NEW (module_reproduction)
      allocate(n_seed(nlen)) !! NEW (module_reproduction)
      allocate(germinated_seeds(nlen)) !! NEW (module_reproduction)
      allocate(acumulated_biomass(nlen)) !! NEW (module_reproduction)
      allocate(max_nppa(nlen)) !! NEW (module_reproduction)




      !     Maximum evapotranspiration   (emax)
      !     =================================
      emax = evpot2(p0,temp,rh,available_energy(temp))
      soil_temp = ts

      !     Productivity & Growth (ph, ALLOCATION, aresp, vpd, rc2 & etc.) for each PLS
      !     =====================
      ! FAZER NUmthreads função de nlen pra otimizar a criação de trheads
      if (nlen .le. 20) then
         call OMP_SET_NUM_THREADS(1)
      else if (nlen .le. 100) then
         call OMP_SET_NUM_THREADS(1)
      else if (nlen .le. 300) then
         call OMP_SET_NUM_THREADS(2)
      else if (nlen .le. 600) then
         call OMP_SET_NUM_THREADS(3)
      else
         call OMP_SET_NUM_THREADS(3)
      endif
      !$OMP PARALLEL DO &
      !$OMP SCHEDULE(AUTO) &
      !$OMP DEFAULT(SHARED) &
      !$OMP PRIVATE(p, ri, carbon_in_storage, testcdef, sr, dt1, mr_sto, growth_stoc, ar_aux)


      do p = 1,nlen

         ! print*, 'survivers', p

         carbon_in_storage = 0.0D0
         testcdef = 0.0D0
         sr = 0.0D0
         ar_aux = 0.0D0
         ri = lp(p)
         dt1 = dt(:,ri) ! Pick up the pls functional attributes list

         height_int(p) = height_aux(ri)
         crown_int(p) = crown_aux(ri)
         ! fpc_grid_int(p) = fpc_grid1(ri)


         call prod(dt1,catm, temp, soil_temp, p0, w, ipar, sla_aux(p),rh, emax&
               &, cl1_pft(ri), ca1_pft(ri), cf1_pft(ri), nleaf(ri), nwood(ri), nroot(ri)&
               &, height_aux(ri), max_height, soil_sat, ph(p), ar(p), nppa(p), laia(p), f5(p), vpd(p), rm(p), rg(p), rc2(p)&
               &, wue(p), c_def(p), vcmax(p), tra(p))

         evap(p) = penman(p0,temp,rh,available_energy(temp),rc2(p)) !Actual evapotranspiration (evap, mm/day)

         !==============================================================================================================================
         !   REPRODUCTION OF TREES [call SEED PRODUCTION MODULE; SEED GERMINATION; UPDATE BIOMASS POOLS; UPDATE SEEDBAK]
         !   (Bruna R. Soica)
         !==============================================================================================================================


         i_day = mod(n_days, real(10, kind=kind(n_days))) + 1
         !i_day = mod(n_days, 10) + 1


         rain_last_days(i_day) = prec
         rain_last_10_days = count(rain_last_days .gt. 0)
         rain_accum_last_10_days = sum(rain_last_days)


         !if (nppa(p) .gt. 0 .and. temp .ge. 23.0 .and. prec .ge. 60) then ! .and. 24.0 .ge. temp .and. temp .le. 33.0 .and. 60.0 .ge. prec .and. prec .le. 200.0) then
         
         if (nppa(p) .gt. 0 .and. temp .ge. 23.0 .and. rain_last_10_days .gt. 0) then
               !print *, "Produção de sementes permitida para PLS", p, "Temperatura:", temp, "Precipitação:", rain_last_10_days &
               !&, "nppa", nppa(p), "Chuva acumulada:", rain_accum_last_10_days
               call repro(nppa(p), height_aux(ri), seed_mass(p), n_seed(p))
               seed_mass_out_budg(ri) = max(seed_mass(p), 0.0D0)

               call repro(nppa(p), height_aux(ri), seed_mass(p), n_seed(p))
               
               seed_mass_out_budg(ri) = max(seed_mass(p), 0.0D0)

            if (n_seed(p) .gt. 0) then

               seed_bank_out_bdgt(ri) = seed_bank_out_bdgt(ri) + nint(stored_seed_bank(ri) + n_seed(p))
            endif

         else
            !print *, "Reprodução não permitida para PLS", p, "Temperatura:", temp, "Precipitação:", prec &
            !&, "nppa", nppa(p)
            seed_bank_out_bdgt(ri) = stored_seed_bank(ri)         
         endif


         if (seed_bank_out_bdgt(ri) .gt. 0 .and. temp .ge. 23.0) then
            germinated_seeds(p) = nint(seed_bank_out_bdgt(ri) * 0.5)
            !print *, "Germinação: ", germinated_seeds(p), "sementes germinadas para PLS", p
            seed_bank_out_bdgt(ri) = nint(seed_bank_out_bdgt(ri) - germinated_seeds(p))
            !print *, "Banco de sementes após germinação:", seed_bank_out_bdgt(ri)
        end if


         !valor máximo para nppa de acordo com as rodadas anteriores
         !max_nppa(p) = 2.774313688   ! 
         max_nppa(p) = 0.93401694

         ! Calcula a biomassa 
         acumulated_biomass(p) = germinated_seeds(p) * seed_mass(p)
         !acumulated_biomass(p) = real(germinated_seeds(p), kind=8) * seed_mass(p)


         ! Verifica se o valor de nppa após incremento ultrapassa o limite máximo
         if (acumulated_biomass(p) .gt. max_nppa(p)) then
            nppa(p) = max_nppa(p)  ! Se ultrapassar o limite, define nppa como o máximo
         else
            nppa(p) = nppa(p) + acumulated_biomass(p)  ! Caso contrário, incrementa normalmente
         end if

         !! ANNUAL SEEDBANK DECAY
         if (n_days .eq. 365 .and. seed_bank_out_bdgt(ri) .gt. 0) then

            decayed_seed_bank(p) = nint(seed_bank_out_bdgt(ri)*0.25)

            seed_bank_out_bdgt(ri) = seed_bank_out_bdgt(ri) - decayed_seed_bank(p)

            !print *, "Tamanho do banco de sementes do PLS n. ", p, " após a decaimento:", seed_bank_out_bdgt(ri)


         endif



         !===================================================================================
         !   END REPRODUCTION
         !===================================================================================

         ! Check if the carbon deficit can be compensated by stored carbon
         carbon_in_storage = sto_budg(1, ri)
         storage_out_bdgt(1, p) = carbon_in_storage
         if (c_def(p) .gt. 0.0) then
            testcdef = c_def(p) - carbon_in_storage
            if(testcdef .lt. 0.0) then
               storage_out_bdgt(1, p) = carbon_in_storage - c_def(p)
               c_def(p) = 0.0D0
            else
               storage_out_bdgt(1, p) = 0.0D0
               c_def(p) = real(testcdef, kind=r_4)       ! testcdef is zero or positive
            endif
         endif
         carbon_in_storage = 0.0D0
         testcdef = 0.0D0

         ! calculate maintanance respirarion of stored C
         mr_sto = sto_resp(temp, storage_out_bdgt(:,p))
         if (isnan(mr_sto)) mr_sto = 0.0D0
         if (mr_sto .gt. 0.1D2) mr_sto = 0.0D0
         storage_out_bdgt(1,p) = max(0.0D0, (storage_out_bdgt(1,p) - mr_sto))

         !     Carbon/Nitrogen/Phosphorus allocation/deallocation
         !     =====================================================

         call allocation (dt1,nppa(p),uptk_costs(ri), soil_temp, w, tra(p)&
            &, mineral_n,labile_p, on, sop, op, cl1_pft(ri),ca1_pft(ri)&
            &, cf1_pft(ri),storage_out_bdgt(:,p),day_storage(:,p),cl2(p),ca2(p)&
            &, cf2(p),litter_l(p),cwd(p), litter_fr(p),nupt(:,p),pupt(:,p)&
            &, lit_nut_content(:,p), limitation_status(:,p), npp2pay(p), uptk_strat(:, p), ar_aux)


         !       CO2 absortion (ES flow indicators (Burkhard et al., 2014))
         !      =============================================================

         call se_module(cl2(p), ca2(p), cf2(p), awood_aux(p), csoil, litter_l(p),&
         & litter_fr(p), cwd(p), co2_abs_se(p))


         ! if (awood_aux(p) .eq. 0.0D0) then
         !    print*, 'GRAMINEA'
         !    print*, 'CO2_ABSORVIDO', co2_abs_se(p)

         ! else
         !    print*, '----- WOOD -------'
         !    print*, 'co2_absorbed', co2_abs_se(p), p
         !    print*, 'CLEAF', cl2(p)
         !    print*, 'CAULE', ca2(p)
         !    print*, 'FINE_ROOT', cf2(p)
         !    print*, 'CSOIL', csoil
         !    print*, '-------------------'
         ! endif

         ! Estimate growth of storage C pool
         ar_fix_hr(p) = ar_aux
         growth_stoc = max( 0.0D0, (day_storage(1,p) - storage_out_bdgt(1,p)))
         if (isnan(growth_stoc)) growth_stoc = 0.0D0
         if (growth_stoc .gt. 0.1D3) growth_stoc = 0.0D0
         storage_out_bdgt(:,p) = day_storage(:,p)

         ! Calculate storage GROWTH respiration
         sr = 0.05D0 * growth_stoc ! g m-2
         if(sr .gt. 1.0D2) sr = 0.0D0
         ar(p) = ar(p) + real(((sr + mr_sto) * 0.365242), kind=r_4) ! Convert g m-2 day-1 in kg m-2 year-1
         storage_out_bdgt(1, p) = storage_out_bdgt(1, p) - sr

         growth_stoc = 0.0D0
         mr_sto = 0.0D0
         sr = 0.0D0

         ! CUE & Delta C
         if(ph(p) .eq. 0.0 .or. nppa(p) .eq. 0.0) then
            cue(p) = 0.0
         else
            cue(p) = nppa(p)/ph(p)
         endif

         delta_cveg(1,p) = cl2(p) - cl1_pft(ri)  !kg m-2
         if(dt1(4) .lt. 0.0D0) then
            delta_cveg(2,p) = 0.0D0
         else
            delta_cveg(2,p) = ca2(p) - ca1_pft(ri)
         endif
         delta_cveg(3,p) = cf2(p) - cf1_pft(ri)

         if(dt1(4) .lt. 0.0D0) then
            delta_biomass(p) = delta_cveg(1,p) + delta_cveg(3,p)
         else
            delta_biomass(p) = delta_cveg(1,p) + delta_cveg(2,p) + delta_cveg(3,p)
         endif

         ! Mass Balance

         if(c_def(p) .gt. 0.0) then
            if(dt1(7) .gt. 0.0D0) then
               cl1_int(p) = cl2(p) - ((c_def(p) * 1e-3) * 0.333333333)
               ca1_int(p) = ca2(p) - ((c_def(p) * 1e-3) * 0.333333333)
               cf1_int(p) = cf2(p) - ((c_def(p) * 1e-3) * 0.333333333)
            else
               cl1_int(p) = cl2(p) - ((c_def(p) * 1e-3) * 0.5)
               ca1_int(p) = 0.0D0
               cf1_int(p) = cf2(p) - ((c_def(p) * 1e-3) * 0.5)
            endif
         else
            if(dt1(7) .gt. 0.0D0) then
               cl1_int(p) = cl2(p)
               ca1_int(p) = ca2(p)
               cf1_int(p) = cf2(p)
            else
               cl1_int(p) = cl2(p)
               ca1_int(p) = 0.0D0
               cf1_int(p) = cf2(p)
            endif
         endif
         if(cl1_int(p) .lt. 0.0D0) cl1_int(p) = 0.0D0
         if(ca1_int(p) .lt. 0.0D0) ca1_int(p) = 0.0D0
         if(cf1_int(p) .lt. 0.0D0) cf1_int(p) = 0.0D0

      enddo ! end pls_loop (p)
      !$OMP END PARALLEL DO

      epavg = emax !mm/day

      ! FILL OUTPUT DATA
      rcavg = 0.0D0
      f5avg = 0.0D0
      laiavg = 0.0D0
      phavg = 0.0D0
      aravg = 0.0D0
      nppavg = 0.0D0
      rmavg = 0.0D0
      rgavg = 0.0D0
      wueavg = 0.0D0
      cueavg = 0.0D0
      litter_l_1 = 0.0D0
      cwd_1 = 0.0D0
      litter_fr_1 = 0.0D0
      c_defavg = 0.0D0
      vcmax_1 = 0.0D0
      specific_la_1 = 0.0D0
      co2_abs_se_1 = 0.0D0
      lit_nut_content_1(:) = 0.0D0
      nupt_1(:) = 0.0D0
      pupt_1(:) = 0.0D0


      cleafavg_pft(:) = 0.0D0
      cawoodavg_pft(:) = 0.0D0
      cfrootavg_pft(:) = 0.0D0
      delta_cveg_1(:, :) = 0.0D0
      storage_out_bdgt_1(:, :) = 0.0D0
      limitation_status_1(:,:) = 0
      uptk_strat_1(:,:) = 0
      npp2pay_1(:) = 0.0
      !seed_bank_out_bdgt(:) = 0.0D0 !! NEW (module_reproduction)


      ! CALCULATE CWM FOR ECOSYSTEM PROCESSES

      ! FILTER NaN in ocupation (abundance) coefficients
      do p = 1, nlen
         if(isnan(ocp_coeffs(p))) ocp_coeffs(p) = 0.0D0
      enddo

      evavg = sum(real(evap, kind=r_8) * ocp_coeffs, mask= .not. isnan(evap))
      phavg = sum(real(ph, kind=r_8) * ocp_coeffs, mask= .not. isnan(ph))
      aravg = sum(real(ar, kind=r_8) * ocp_coeffs, mask= .not. isnan(ar))
      nppavg = sum(real(nppa, kind=r_8) * ocp_coeffs, mask= .not. isnan(nppa))
      laiavg = sum(laia * ocp_coeffs, mask= .not. isnan(laia))
      rcavg = sum(real(rc2, kind=r_8) * ocp_coeffs, mask= .not. isnan(rc2))
      f5avg = sum(f5 * ocp_coeffs, mask= .not. isnan(f5))
      rmavg = sum(real(rm, kind=r_8) * ocp_coeffs, mask= .not. isnan(rm))
      rgavg = sum(real(rg, kind=r_8) * ocp_coeffs, mask= .not. isnan(rg))
      wueavg = sum(real(wue, kind=r_8) * ocp_coeffs, mask= .not. isnan(wue))
      cueavg = sum(real(cue, kind=r_8) * ocp_coeffs, mask= .not. isnan(cue))
      c_defavg = sum(real(c_def, kind=r_8) * ocp_coeffs, mask= .not. isnan(c_def)) / 2.73791
      vcmax_1 = sum(vcmax * ocp_coeffs, mask= .not. isnan(vcmax))
      specific_la_1 = sum(specific_la * ocp_coeffs, mask= .not. isnan(specific_la))
      litter_l_1 = sum(litter_l * ocp_coeffs, mask= .not. isnan(litter_l))
      cwd_1 = sum(cwd * ocp_coeffs, mask= .not. isnan(cwd))
      litter_fr_1 = sum(litter_fr * ocp_coeffs, mask= .not. isnan(litter_fr))
      c_cost_cwm = sum(npp2pay * ocp_coeffs, mask= .not. isnan(npp2pay))
      co2_abs_se_1 = sum(co2_abs_se * ocp_coeffs, mask= .not. isnan(co2_abs_se))*10

      cp(1) = sum(cl1_int * ocp_coeffs, mask= .not. isnan(cl1_int))
      cp(2) = sum(ca1_int * (ocp_coeffs * idx_grasses), mask= .not. isnan(ca1_int))
      cp(3) = sum(cf1_int * ocp_coeffs, mask= .not. isnan(cf1_int))
      cp(4) = sum(ar_fix_hr * (ocp_coeffs * idx_pdia), mask= .not. isnan(ar_fix_hr))

      !print*, 'CO2_ABS (t/ha)', co2_abs_se_1


      ! FILTER BAD VALUES
      do p = 1,2
         do i = 1, nlen
            if (isnan(nupt(p, i))) nupt(p, i) = 0.0D0
            if (nupt(p, i) .gt. 1.5D2) nupt(p, i) = 0.0D0
            if (nupt(p, i) .lt. 0.0D0) nupt(p, i) = 0.0D0
         enddo
      enddo

      do p = 1,3
         do i = 1, nlen
            if(isnan(pupt(p, i))) pupt(p, i) = 0.0D0
            if (pupt(p, i) .gt. 0.7D2) pupt(p, i) = 0.0D0
            if (pupt(p, i) .lt. 0.0D0) pupt(p, i) = 0.0D0
         enddo
      enddo

      do p = 1,2
         nupt_1(p) = sum(nupt(p,:) * ocp_coeffs)
      enddo
      do p = 1,3
         pupt_1(p) = sum(pupt(p,:) * ocp_coeffs)
      enddo

      do p = 1,6
         do i = 1, nlen
            if(isnan(lit_nut_content(p, i))) lit_nut_content(p, i) = 0.0D0
            if (lit_nut_content(p, i) .gt. 1.0D2) lit_nut_content(p, i) = 0.0D0
            if (lit_nut_content(p, i) .lt. 0.0D0) lit_nut_content(p, i) = 0.0D0
         enddo
      enddo

      do p = 1,3
         do i = 1, nlen
            if(isnan(storage_out_bdgt(p,i))) storage_out_bdgt(p,i) = 0.0D0
            if(storage_out_bdgt(p,i) > 0.5D2) storage_out_bdgt(p,i) = 0.0D0
            if(storage_out_bdgt(p,i) < 0.0D0) storage_out_bdgt(p,i) = 0.0D0
         enddo
      enddo

      do p = 1, 6
         lit_nut_content_1(p) = sum(lit_nut_content(p, :) * ocp_coeffs)
      enddo

      do p = 1, nlen
         ri = lp(p)

         cleafavg_pft(ri)  = cl1_int(p)
         cawoodavg_pft(ri) = ca1_int(p)
         cfrootavg_pft(ri) = cf1_int(p)
         delta_cveg_1(:,ri) = delta_cveg(:,p)
         storage_out_bdgt_1(:,ri) = storage_out_bdgt(:,p)
         limitation_status_1(:,ri) = limitation_status(:,p)
         uptk_strat_1(:,ri) = uptk_strat(:,p)
         uptk_strat_1(:,ri) = uptk_strat(:,p)
         npp2pay_1(ri) = npp2pay(p)
         !stored_seed_bank(ri) = seed_bank_out_bdgt(p) !! NEW (module_reproduction)


      enddo

      deallocate(lp)
      deallocate(evap)
      deallocate(nppa)
      deallocate(ph)
      deallocate(ar)
      deallocate(laia)
      deallocate(f5)
      deallocate(f1)
      deallocate(vpd)
      deallocate(rc2)
      deallocate(rm)
      deallocate(rg)
      deallocate(wue)
      deallocate(cue)
      deallocate(c_def)
      deallocate(vcmax)
      deallocate(specific_la)
      deallocate(storage_out_bdgt)
      deallocate(tra)
      deallocate(nupt)
      deallocate(pupt)
      deallocate(litter_l)
      deallocate(cwd)
      deallocate(litter_fr)
      deallocate(lit_nut_content)
      deallocate(delta_cveg)
      deallocate(npp2pay)
      deallocate(limitation_status)
      deallocate(uptk_strat)
      deallocate(cl1_int)
      deallocate(cf1_int)
      deallocate(ca1_int)
      deallocate(cl2)
      deallocate(cf2)
      deallocate(ca2)
      deallocate(day_storage)
      DEALLOCATE(idx_grasses)
      DEALLOCATE(idx_pdia)
      DEALLOCATE(ar_fix_hr)
      deallocate(height_int)
      deallocate(crown_int)
      deallocate(co2_abs_se)
      deallocate(seed_bank_int_repro) !! NEW (module_reproduction)
      deallocate(seed_bank_out_repro) !! NEW (module_reproduction)
      deallocate(decayed_seed_bank) !! NEW (module_reproduction)
      deallocate(n_seed) !! NEW (module_reproduction)
      deallocate(germinated_seeds) !! NEW (module_reproduction)
      deallocate(acumulated_biomass) !! NEW (module_reproduction)
      deallocate(max_nppa)



   end subroutine daily_budget

end module budget
