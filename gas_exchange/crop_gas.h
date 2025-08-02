// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the PLANT_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// PLANT_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.

// See creating a pocket PC dll using C++ article for information
// on how to do this
#ifdef _WIN32
#ifdef MYPLANT_EXPORTS
#define PLANT_API __declspec(dllexport)
#else
#define PLANT_API __declspec(dllexport)
#endif
#else
#define PLANT_API
#endif
#include "initInfo.h"




TInitInfo	initInfo;

//Common Structures defined here

#pragma pack(2)
struct WeatherCommon {

	int   CDayOfYear, CITIME, CIPERD, WATLIMIT;  // Day of the year, itime: hour 1 to 24, ciperd: 24, water limited = 1 if actual, 0 if potential
	float CWATTSM[24], Cpar[24], CTAIR[24], CCO2, CVPD[24], CWIND, CPSIL_, CLATITUDE, CLONGITUDE, CALTIDUDE, CLAREAT, CLAI;
	//CWATTSM: Wattsm in each hour 
	//PAR: photosynthetically active radiation in each hour
	//TAIR:Air temperature in each hour
	//CO2: Carbon Dioxide
	//CVPD:Water vapour pressure deficit
	//CWInd: WInd for the day
	//Cpsil_minimum leaf water potential for the day
	//Clatitude:Latude from climatefile
	//Clareat:LAI/ (poparea/100*100)    poparea: population /m2 
	//Clai: leaf area index

};

struct PlantCommon {

	float NRATIO, photosynthesis_gross, sunPg, shadePg, photosynthesis_grosssunlitleaf, photosynthesis_grossshadedleaf, photosynthesis_net, sunlitPFD, shadedPFD,
		sunlitLAI, shadedLAI, transpiration, transpiration_sunlitleaf, transpiration_shadedleaf, conductance, temperature, temp1, Ags, ARH, EET, cVCMAX, cJMAX, cTPU,
		cg0, cg1;
};


#pragma pack()



#ifdef __cplusplus
extern "C" {
#endif

	// Your exported function headers go here 
	// GASEXCHANGER must be upper and lower case because it is a function name
#ifdef _WIN32
	PLANT_API void _stdcall GASEXCHANGER(struct WeatherCommon*, PlantCommon*);
#else
	PLANT_API void GASEXCHANGER_(struct WeatherCommon*, PlantCommon*); ,
#endif
#ifdef __cplusplus
}
#endif




