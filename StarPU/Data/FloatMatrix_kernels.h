void cuda_floatmatrix_add(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC);

void cuda_floatmatrix_sub(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC);

void cuda_floatmatrix_duplicate(unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB);
