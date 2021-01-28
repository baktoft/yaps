// nll contrib for top when pingType == 'sbi'
	PARAMETER(logSigma_bi);		// Sigma for burst interval
	Type sigma_bi = exp(logSigma_bi);

	nll -= dnorm(top(0),Type(0.0),Type(4.0),true);
	nll -= dnorm(top(1),Type(approxBI),Type(4.0),true);
	for(int i = 2; i < np; ++i)	{
		nll -= dnorm(top(i)-2*top(i-1)+top(i-2), Type(0), sigma_bi, true);
	}
