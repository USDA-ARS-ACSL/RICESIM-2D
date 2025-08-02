// 5/06/2020 WSun designed a gas exchange module using dll which connect the original GLYCIM
// 5/08/2020 WSun modified the program by using transpiration of photosynthesis module to calculate EO
// 5/09/2020 WSun deleted the ET_supply which not used in the photosynthesis
// 5/10/2020 WSun deleted the parameters (Tubmod, Feedback, leafageeffect) which was used in SPUDSIM (might added in future)
// 5/11/2020 WSun give a reasonable longitude and reset conductance
// 5/17/2020 WSun applied Ball-Berry model stomatal conductance values (g0 and g1)
// 5/18/2020 WSun revised the code in PNET rountines which calculate the USEDCS 

// crop.cpp : Defines the entry point for the DLL application
//#define MYPLANT_EXPORTS

#include "pch.h"
#include "crop_gas.h"
#include "controller.h"
#include "weather.h"

using namespace std;
#include <cmath>

#define endl "\n"
#define comma ","

// note that we have to dereference the variable in order to
// assign a value that can be passed back to GLYCIM. This is 
// because the FORTRAN program expects a pointer rather than
// a value. I don't think this applies to structures as it does 
// variables that may be in the arguments list.
// note use of lower case names. Upper and lower case conversions between
// fortran and C++ don't matter here because these are arguments and not
// a function name. GASEXCHANGER must be upper case because it is a function name

int compare(const void* arg1, const void* arg2)
{
	/* Compare all of both strings: */
	if (*(double*)arg1 > *(double*)arg2) return 1;
	else if (*(double*)arg1 < *(double*)arg2) return -1;
	return 0;
};
#ifdef _WIN32
void _stdcall GASEXCHANGER(struct
#else
void GASEXCHANGER(struct
#endif
	WeatherCommon* Weather,   // Define global variables block which connect GLYCIM variables
	PlantCommon* Plant

)

{


	static CController* pSC; //declare as static to ensure only one copy is instantiated during GLYCIM execution

	//	TInitInfo initInfo;
	{

		initInfo.latitude = Weather->CLATITUDE;
		initInfo.longitude = Weather->CLONGITUDE;
		initInfo.altitude = Weather ->CALTIDUDE;      
		initInfo.NRATIO = Plant->NRATIO;
		initInfo.cVCMAX = Plant->cVCMAX;
		initInfo.cJMAX = Plant->cJMAX;
		initInfo.cTPU = Plant->cTPU;
		initInfo.cg0 = Plant->cg0;
		initInfo.cg1 = Plant->cg1;

		//Weather->PSIL[Weather->ITIME-1] = -2.0;

		pSC = new CController(initInfo); // RESET variables at the beginning of each run (1 hour) // goes to controller.cpp
	}


	double Es;

	TWeather wthr;  // goes to weather.h  // Tweather is the structure // reset all the initial values
	{
		wthr.DayOfYear = Weather->CDayOfYear;
		//wthr.time = ((Weather->CITIME) - 1) * (1.0 / (Weather->CIPERD));  //not sure this will be used
		wthr.time = ((Weather->CITIME) - 1) * 0.0416667;
		wthr.CO2 = Weather->CCO2;
		//wthr.airT = Weather->TAIR[Weather->ITIME];
		wthr.airT = Weather->CTAIR[Weather->CITIME - 1];
		wthr.PFD = Weather->Cpar[Weather->CITIME - 1] * 4.55; // conversion from PAR in W m-2 to umol s-1 m-2
		wthr.solRad = Weather->CWATTSM[Weather->CITIME - 1]; //conversion from W m-2 total radiation to J m-2 in one hour 				
		double Es = (0.611 * exp(17.502 * wthr.airT / (240.97 + wthr.airT))); // saturated vapor pressure at airT
		wthr.RH = (1 - (Weather->CVPD[Weather->CITIME - 1] / Es)) * 100.0; // relative humidity in percent
		wthr.wind = Weather->CWIND * (1000.0 / 3600.0); // conversion from km hr-1 to m s-1
		wthr.psil_ = Weather->CPSIL_;//CPSIL_ is already in MPa 
		wthr.LAI = Weather->CLAI;
		//wthr.LAI = Weather->LAREAT * 38.75196 * 1.0E-4; // LAI = LAREAT * POPARE * 1.0E-4
		//wthr.ET_supply = 0.0;
		wthr.wpotential = Weather->WATLIMIT;

		/******************************/

	}


	// A gas exchange module object is initialized (calls function) here
	//  ***************************************************************************


	pSC->run(wthr, initInfo);   // goes to controller.cpp

	//DHF - removed Wenguang's conversions. I prefer to convert in crop model portion so I can verify original variables are working as expected

	Plant->photosynthesis_gross = pSC->get_photosynthesis_gross();//umol CO2 m-2 ground s-1 
	Plant->photosynthesis_net = pSC->get_photosynthesis_net();//umol CO2 m-2 ground s-1 
	Plant->photosynthesis_grosssunlitleaf = pSC->get_photosynthesis_grosssunlitleaf(); //umol co2 m-2 leaf s-1
	Plant->photosynthesis_grossshadedleaf = pSC->get_photosynthesis_grossshadedleaf(); //umol co2 m-2 leaf s-1
	Plant->transpiration = pSC->get_transpiration(); //mmol h2o m-2 ground s-1
	Plant->temperature = pSC->get_temperature(); // return back leaf temperature 
	//Plant->TLAI = pSC->get_TLAI();
	Plant->transpiration_sunlitleaf = pSC->get_transpiration_sunlitleaf(); //mmol h2o m-2 leaf s-1
	Plant->transpiration_shadedleaf = pSC->get_transpiration_shadedleaf(); //mmol h2o m-2 leaf s-1
	Plant->sunlitLAI = pSC->get_sunlitLAI();
	Plant->shadedLAI = pSC->get_shadedLAI();
	Plant->sunlitPFD = pSC->get_sunlitPFD();
	Plant->shadedPFD = pSC->get_shadedPFD();
	Plant->temp1 = pSC->get_temp1();
	Plant->Ags = pSC->get_Ags();
	Plant->ARH = wthr.RH;
	Plant->conductance = pSC->get_conductance();
	Plant->EET = pSC->get_EET();
	Plant->sunPg = pSC->get_sunPg();
	Plant->shadePg = pSC->get_shadePg();
	return;

}

