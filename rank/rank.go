//This contains functions to make ranking corps and processing scores easier
package rank

import (
	"github.com/EMurray16/Rgo/sexp"
	"sort"
	//"fmt"
)

//create a type to hold corps names
type CorpsNames struct {
	Ind2name map[int]string
	Name2ind map[string]int
}

//create a scores type that can be sorted retaining indices
type Scores struct {
	sort.Float64Slice
	ind []int
}

func (s Scores) Swap(i, j int) {
	s.Float64Slice.Swap(i, j)
	s.ind[i], s.ind[j] = s.ind[j], s.ind[i]
}
func NewScores(scores ...float64) *Scores {
	nscore := len(scores)
	s := &Scores{Float64Slice: sort.Float64Slice(scores), ind: make([]int, nscore)}
	for i, _ := range s.ind {
		//reverse the order to make sorting naturally descending
		s.ind[i] = nscore - i
	}
	return s
}

//this function returns the ranks given several scores
func PredReduce(scoremat sexp.Matrix) (resmat sexp.Matrix) {
	//start by finding the number of corps
	Ncorps := scoresexp.Ncol
	Nmonte := scoresexp.Nrow
	weight := float64(1) / float64(Nmonte)

	//now make the reduction matrix
	//the rows are, in order: mean, percGold, percSilver, percBronze, percFinals, percSemis
	redmat := sexp.CreateZeros(6, Ncorps)

	//loop through the runs and sort to make
	for r := 1; r <= Nmonte; r++ {
		rawscores := scoresexp.GetRow(r)

		//add to the totals to get the mean later
		for ind, score := range rawscores {
			redsexp.AddInd(1, ind+1, score*weight)
		}

		//now make the scores sortable and sort them
		sortscores := NewScores(rawscores...)
		sort.Sort(sortscores)

		//now increment the gold, silver, and bronze counters
		redsexp.AddInd(2, sortscores.ind[0], weight)
		redsexp.AddInd(3, sortscores.ind[1], weight)
		redsexp.AddInd(4, sortscores.ind[2], weight)

		//increment the finals counters
		for i := 0; i < 24; i++ {
			redsexp.AddInd(6, sortscores.ind[i], weight)
			if i < 12 {
				redsexp.AddInd(5, sortscores.ind[i], weight)
			}
		}
	}

	//now we can return
	resmat = *redmat
	return resmat
}
