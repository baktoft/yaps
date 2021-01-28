	PARAMETER(logSigma_toa);	// Sigma TimeOfArrival
	Type sigma_toa = exp(logSigma_toa);

	for(int i=0; i<np; ++i){ //iterate pings
		for(int h=0; h<nh; ++h){ //iterate hydros
			if(!isNA(toa(h,i))){ //ignore NA's...
				nll -= Edist(0) * dnorm(eps(h,i), Type(0), sigma_toa, true); 					//Gaussian part					
			}
		}
	}


