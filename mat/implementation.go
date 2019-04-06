//This is a matrix implementation for the DCI 2019 model
package mat

//create the matrix type
type Matrix struct {
	Nrow, Ncol int
	V          []float64
}

//this method returns an entire row of the matrix as a slice
func (m *Matrix) GetRow(ind int) (f []float64) {
	//the row indexes are next to each other
	startind := (ind - 1) * m.Ncol
	f = m.V[startind:(startind + m.Ncol)]
	return f
}

//this method returns an entire column of the matrix as a slice
func (m *Matrix) GetCol(ind int) (f []float64) {
	//the col indexes are not contiguous, so preallocate the slice
	f = make([]float64, m.Nrow)

	//now loop through the matrix and add elements one by one
	sliceind := 0
	for i := ind - 1; sliceind < m.Nrow; i += m.Ncol {
		f[sliceind] = m.V[i]
		sliceind++
	}

	return f
}

//this method gets a single index of the matrix
func (m *Matrix) GetInd(rowi, coli int) float64 {
	i := (rowi-1)*m.Ncol + (coli - 1)
	return m.V[i]
}

//this method sets the entire row of a matrix
func (m *Matrix) SetRow(ind int, data []float64) {
	startind := (ind - 1) * m.Ncol
	//edit the row in a loop
	dataind := 0
	for i := startind; i < startind+m.Ncol; i++ {
		m.V[i] = data[dataind]
		dataind++
	}
}

//this method sets the entire column of a matrix
func (m *Matrix) SetCol(ind int, data []float64) {
	sliceind := 0
	for i := ind - 1; sliceind < m.Nrow; i += m.Ncol {
		m.V[i] = data[sliceind]
		sliceind++
	}
}

//this method sets a single index of a matrix
func (m *Matrix) SetInd(rowi, coli int, data float64) {
	i := (rowi-1)*m.Ncol + (coli - 1)
	m.V[i] = data
}

//this function creates a zero-value matrix
func CreateZeros(nrow, ncol int) *Matrix {
	//create the vector
	v := make([]float64, nrow*ncol)
	//create the matrix
	m := Matrix{nrow, ncol, v}

	return &m
}
