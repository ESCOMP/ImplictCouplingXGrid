# ImplictCouplingXGrid
Application demonstrating ESMF exchange grid with implicit atmosphere-land coupling

1. both ATM and LND transfer grids to MED

2. XGrid is created in MED from ATM grid and LND grid. 

3. Both ATM and LND transfer surface flux relevant quantities to MED

4. Two sweep calculation for flux update

Note:

Tested with ESMF version hash: 8.1bs36 (8.1 series)

Run mpirun -np 4 ./app
