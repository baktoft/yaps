	PARAMETER(logScale);		// scale-parameter for t-dist
	Type scale = exp(logScale);

	for(int i=0; i<np; ++i){ //iterate pings
		for(int h=0; h<nh; ++h){ //iterate hydros
			if(!isNA(toa(i,h))){ //ignore NA's...
				nll -= E_dist_vec(2) * log(dt(eps(i,h)/scale, Type(3.0), false)/scale);		// t
			}
		}
	}


