# ImplictCouplingXGrid
Application demonstrating ESMF exchange grid with implicit atmosphere-land coupling

1. both ATM and LND transfer grids to MED

2. XGrid is created in MED from ATM grid and LND grid. 

3. Both ATM and LND transfer surface flux relevant quantities to MED

4. Two sweep calculation for flux update

Note:

Tested with ESMF version hash: 406ff0a9ec7e157ce45853da1264e4ecca668e1e (8 series)

Run mpirun -np 4 ./app
