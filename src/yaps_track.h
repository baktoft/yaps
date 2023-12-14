	DATA_ARRAY(H);			// Position of hydros 
	DATA_ARRAY(toa);   		// Time of arrival at hydro. One row per buoy, one column per ping
	DATA_INTEGER(nh);
	DATA_INTEGER(np);
	DATA_STRING(ping_type);
	DATA_SCALAR(bi_epsilon);    
	DATA_SCALAR(bi_penalty);
	DATA_SCALAR(rbi_min);
	DATA_SCALAR(rbi_max);
	DATA_STRING(ss_data_what);	// 'est' = estimate SS-data; 'data' = Use SS-data
	DATA_VECTOR(ss_data);		// Vector of SS-data if used - length(ss_data) = np
	DATA_IVECTOR(ss_idx);
	DATA_INTEGER(n_ss);
	// DATA_SCALAR(approxBI);

	DATA_VECTOR(Edist);
	DATA_VECTOR(biTable);
	DATA_VECTOR(bbox);

	DATA_STRING(how_3d);
	DATA_VECTOR(z_vec);
	
	
	PARAMETER_VECTOR(X);	//Position at time of ping
	PARAMETER_VECTOR(Y);	//Position at time of ping
	PARAMETER_VECTOR(TOP);		// Estimated time of pings

	PARAMETER(logD_xy);    		// Diffusivity of fish
	Type D_xy = exp(logD_xy);
	

	array<Type> mu_toa(nh,np);  // mu-matrix
	array<Type> delta_t(nh,np);  // matrix for delta toa - top
	array<Type> dist(nh,np);	// dist-matrix
	array<Type> eps(nh,np);		// eps-matrix
	vector<Type> ss_i(np);

	Type nll = 0.0;
	
	if(ss_data_what == "est"){
		#include "nll_ss_est.h"
	} else {
		#include "nll_ss_data.h"
	}
	
	if(how_3d != "est"){
		for(int i=0; i<np; ++i){ //iterate pings
			for(int h=0; h<nh; ++h){ //iterate hydros
				if(!isNA(toa(h,i))){ //ignore NA's...
					if(how_3d == "none"){
						dist(h,i) = sqrt((H(h,0)-X(i))*(H(h,0)-X(i)) + (H(h,1)-Y(i))*(H(h,1)-Y(i)));
					} else if(how_3d == "data"){
						dist(h,i) = sqrt((H(h,0)-X(i))*(H(h,0)-X(i)) + (H(h,1)-Y(i))*(H(h,1)-Y(i)) + (H(h,2)-z_vec(i))*(H(h,2)-z_vec(i)));
					}
					mu_toa(h,i) = TOP(i) +  dist(h,i)/ss_i(i);
					eps(h,i) = toa(h,i) - mu_toa(h,i);
					
					// // Making sure that all toas are later than top
					// delta_t(h,i) = (toa(h,i) - top(i));
					// nll += bi_penalty * (softplus(0 - delta_t(h,i), bi_epsilon));
					// nll += Type(1E6) * (softplus(delta_t(h,i) - 2, Type(1E-6)) + softplus(-1 - delta_t(h,i), Type(1E-6)));
				}
			}
		}
	} else {
		#include "dist_mat_3d_est.h"
	}
	
	// error distribution...
	if(Edist(0) ==1){
		#include "nll_gaus-dist.h"
	} else if (Edist(1) == 1){
		#include "nll_mix-dist.h"
	} else if(Edist(2) == 1){
		#include "nll_t-dist.h"
	}

	//position component
	nll -= dnorm(X(0),Type(0),Type(10000),true);
	nll -= dnorm(Y(0),Type(0),Type(10000),true);
	for(int i=1; i<np; ++i)	{
		nll -= dnorm(X(i), X(i-1),sqrt(2*D_xy*(TOP(i) - TOP(i-1))),true);	
		nll -= dnorm(Y(i), Y(i-1),sqrt(2*D_xy*(TOP(i) - TOP(i-1))),true);

		// nll -= dnorm(X(i),Type(0),Type(10000),true);
		// nll -= dnorm(Y(i),Type(0),Type(10000),true);
		
	}
	
	// spatial constraints
	if(!isNA(bbox(0))){
	    #include "nll_bbox.h"
	}

	//burst interval component
	if(ping_type == "sbi"){
	    #include "nll_pingtype_sbi.h"
	} else if(ping_type == "sbi_double"){
		#include "nll_pingtype_sbi_double.h"
	} else if(ping_type == "rbi"){
		#include "nll_pingtype_rbi.h"
	} else if(ping_type == "pbi"){
		#include "nll_pingtype_pbi.h"
	}
	// make sure all pings come in correct order...
	for(int i=1; i<np; ++i){
		nll += bi_penalty * (softplus(0 - (TOP(i) - TOP(i-1)), bi_epsilon));
	}


	REPORT(mu_toa);
	
	return nll;
