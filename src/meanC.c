void meanC(double *vector, int *val, int *size){
	int i = 0;

	for (i = 0; i < *size; i++){
		*val = *val + vector[i];
	}

	*val = (*val / *size);
}
