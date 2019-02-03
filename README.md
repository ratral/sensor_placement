# Hydraulic model to understand the reduction of Real Water Losses in Water Distribution System (WDS). 

The idea behind this project is to develop a standard hydraulic model to demonstrate the effects of pressure management and the different models of leakage detection and location in Water Distribution Systems (WDSs). For this we will use EPANET with R and the packages [epanetReader](https://github.com/cran/epanetReader) and [epanet2toolkit](https://github.com/bradleyjeck/epanet2toolkit) (thanks to the developers).

First, we need to create a fictional Hydraulic modelling network sector with measured inputs and for pressure reducing valve ontroller.

Second, We will define a fictitious _**District Measured Area (DMA)**_ to check the different strategies of pressure management in order to reduce the water losses. DMAs are discrete areas of the _**water distribution system (WDS)**_ and are created by closing boundary valves or by permanently disconnecting pipes to other areas so that it remains flexible to changing demands.

An effective undertanding, measurement and calculation of the diferent modells and strategien will help to quantify un-generated water and reduce waste.

Tasks:

 - Development of a standard distribution network (PRV_01.inp)
 - Water Demand Patterns:
     - Different periods of the year (Spring, Summer, Fall, Winter and Summerbreak);
     - Two different types of day for each time period exist (working-days and holidays-weekends). 
 - Monte Carlo Simulation of the diferent Demand Patterns
 - Report (report_prv_01.Rmd) 


## The effect of pressure on leakage Water Distribution System (WDS).

One of the major factors influencing leakage is the pressure in the distribution system. In the past the conventional view was that leakage from water distribution systems is relatively insensitive to pressure, as described by the orifice equation:

<img src="https://latex.codecogs.com/svg.latex?q_t=C_d*A*{\sqrt{2gh}}"/>

where _qt_ the flowrate, _C_ the discharge coefficient, _A_ the orifice area, _g_ acceleration due to gravity and h the pressure head. To apply this equation to leaks in pipes it can be written in more general form as: 
 
<img src="https://latex.codecogs.com/svg.latex?q_t=C*p^{\alpha}"/>

where c is the leakage coefficient and α the leakage exponent. A number of field studies have shown that α can be considerably larger than 0.5, and typically varies between 0.5 and 2.79 with a median of 1.15.


## Indicators for Real Losses and non-revenue water 

 - **(litres) / (conetion) / (day) / (m pressure)**
 - **(litres) / (km pipe) / (day) /  (m pressure)**
 - **ILI** Ratio of Current Annual Real Losses to Unavoidable Annual Real Losses, most powerful indicator for comparisons between systems.
 
## Pressure Management

Calculation of the effect of different pressure management models.

Pressure management can be defined as the practice of managing system pressures to the optimum levels of service while ensuring sufficient and efficient supply to legitimate uses.The positive effects of pressure management are to decrease real water losses by reducing unnecessary or excess pressures.

**Tasks for One Inlet in DMAs systems**
One Inlet with one Pressure Reduction Valve (PRV), no Outlets
 - PRV mit constant pressure;
 - PRV with Time-based pressure modulation;
 - PRV with controlled pressure according to the flow;

**Tasks for Multi inlet in DMAs systems**
Mohre as one inlet with PRVs and posible Outlet with pressure sostinable Valve PSV 
- Teil Tank : Water inlet from a tank in head and tail locations of the DMA
- Multi inlets with PRVs 
- Multi Inlets with PRVs and Outlet with PSV

**Analise of the Tank Effect**
The idea is to use the tanks of the users as shock absorbers of consumption.


## Comparison of different sensor placement algorithms for model-based leak pre-localization.

The term "Leakage awareness methods" ist used to explain the discovery of a leak in a particular area within the network. It does not give any information about its precise location. 

For the leak awareness method is a hidraulic model needed. Different type hydraulic models have been proposed to detect leaks in WDS. Those methods usually involve calibration/optimisation techniques to analyse the different areas of the network. 

The calibration/optimisation requires the installation of flow meters and pressure sensors at specific points in the DMAs. The inlet flow into and out of the DMAs need to be metered. 

The aim of this hidraulic models is to reduce the area where leakage is occurring in order to make later pinpointing easier. A traditional approach is to divide the network into DMAs.





 
