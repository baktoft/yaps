	PARAMETER(logSigma_toa);	// Sigma TimeOfArrival
	Type sigma_toa = exp(logSigma_toa);

	PARAMETER(logScale);		// scale-parameter for t-dist
	Type scale = exp(logScale);

	PARAMETER(log_t_part);		// t-part of mixture model 
	Type t_part = exp(log_t_part);
	Type G_part = Type(1.0) - t_part; //Gaussian part of mixture model


	for(int i=0; i<np; ++i){ //iterate pings
		for(int h=0; h<nh; ++h){ //iterate hydros
			if(!isNA(toa(h,i))){ //ignore NA's...
				nll -= Edist(1) * log( G_part * dnorm(eps(h,i), Type(0),sigma_toa,false) + 		//Gaussian part
					t_part * dt(eps(h,i)/scale, Type(3.0), false) );					//t part
			}
		}
	}


