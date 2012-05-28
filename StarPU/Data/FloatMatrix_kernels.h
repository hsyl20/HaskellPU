void cuda_floatmatrix_add(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC);

void cuda_floatmatrix_sub(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC);

void cuda_floatmatrix_duplicate(unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB);

void cuda_floatmatrix_submatrix(unsigned x, unsigned y, unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB);

void cuda_floatmatrix_set(unsigned w, unsigned h, float v, const float* A, unsigned ldA);

void cuda_floatmatrix_transpose(unsigned w, unsigned h, float* A, unsigned ldA, float* B, unsigned ldB);

void cuda_floatmatrix_scale(unsigned w, unsigned h, float value, float* A, unsigned ldA, float* B, unsigned ldB);
