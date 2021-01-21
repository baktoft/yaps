// nll contrib if bbox constraints are specified
	Type x_min = bbox(0);
	Type x_max = bbox(1);
	Type y_min = bbox(2);
	Type y_max = bbox(3);
	Type eps   = bbox(4);
	Type pen   = bbox(5);
	
	for(int i=0; i<np; i++){
		nll += pen * (
			softplus(X(i) - x_max, eps) + softplus(x_min - X(i), eps) +
			softplus(Y(i) - y_max, eps) + softplus(y_min - Y(i), eps)
		);
	}
