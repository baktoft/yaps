// nll contrib for top when pingType == 'pbi'
	PARAMETER(logSigma_bi);		// Sigma for burst interval
	Type sigma_bi = exp(logSigma_bi);
	
	PARAMETER_VECTOR(tag_drift);

	top_pbi(0) = Type(0.0);
	for(int i = 1; i < np; ++i)	{
		top_pbi(i) = top_pbi(i-1) + biTable(i-1);
	}
	for(int i = 0; i < np; ++i)	{
		nll -= dnorm(top(i), top_pbi(i) + tag_drift(i), Type(1E-6), true);
	}


	nll -= dnorm(tag_drift(0), Type(0.0), Type(4), true);
	nll -= dnorm(tag_drift(1), Type(0.0), Type(4), true);
	for(int i = 2; i < np; ++i)	{
		nll -= dnorm(tag_drift(i)-2*tag_drift(i-1)+tag_drift(i-2), Type(0), sigma_bi, true);
	}
