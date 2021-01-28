	PARAMETER(logScale);		// scale-parameter for t-dist
	Type scale = exp(logScale);

	for(int i=0; i<np; ++i){ //iterate pings
		for(int h=0; h<nh; ++h){ //iterate hydros
			if(!isNA(toa(h,i))){ //ignore NA's...
				nll -= Edist(2) * log(dt(eps(h,i)/scale, Type(3.0), false)/scale);		// t
			}
		}
	}


