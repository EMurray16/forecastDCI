//This contains methods for adding/subtracting elements of matrices
package mat

//import "fmt"

//this method adds to all elements of a row
func (m *Matrix) AddRow(ind int, data []float64) {
	//start by getting the row of the matrix
	row := m.GetRow(ind)

	//now add each element of data to row
	for i, v := range row {
		row[i] = v + data[i]
	}

	//now set that row
	m.SetRow(ind, row)
}

//this method adds to all elements of a column
func (m *Matrix) AddCol(ind int, data []float64) {
	//start by getting the col of the matrix
	col := m.GetCol(ind)

	//now add each element of data to col
	for i, v := range col {
		col[i] = v + data[i]
	}

	//now set that col
	m.SetCol(ind, col)
}

//this method adds to a single index of the matrix
func (m *Matrix) AddInd(rowi, coli int, data float64) {
	//get the index
	i := (rowi-1)*m.Ncol + (coli - 1)
	m.V[i] = m.V[i] + data
}

//this method subtracts from a row of the matrix
func (m *Matrix) SubRow(ind int, data []float64) {
	//start by getting the row of the matrix
	row := m.GetRow(ind)

	//now add each element of data to row
	for i, v := range row {
		row[i] = v - data[i]
	}

	//now set that row
	m.SetRow(ind, row)
}

//this method subtracts from all elements of a column
func (m *Matrix) SubCol(ind int, data []float64) {
	//start by getting the col of the matrix
	col := m.GetCol(ind)

	//now add each element of data to col
	for i, v := range col {
		col[i] = v - data[i]
	}

	//now set that col
	m.SetCol(ind, col)
}

//this method subtracts from a single index of the matrix
func (m *Matrix) SubInd(rowi, coli int, data float64) {
	//get the index
	i := (rowi-1)*m.Ncol + (coli - 1)
	m.V[i] = m.V[i] - data
}
