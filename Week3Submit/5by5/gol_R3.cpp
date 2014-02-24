#include <iostream>
#include <bitset>
#include "Rcpp.h"

// -------------------------------------------------------------- //
// Convenience
// -------------------------------------------------------------- //

// print a progress percentage
void Progress(const int num_total, const int num_processed)
{
    // pogress
    static int i_permille_old = -999; 
    int i_permille = static_cast<int>(floor(1000 * (num_total+2)/float(num_processed)));
    if (i_permille != i_permille_old)
    {
        std::cout << "  \015\033[32m ---> \033[1m\033[31m" << i_permille/10.0 << "\033[0m\033[32m <---\033[0m\015";
        //printf("  \015\033[32m ---> \033[1m\033[31m%4.1f%%\033[0m\033[32m <---\033[0m\015", i_permille/10.0);
        fflush(stdout);
        i_permille_old = i_permille;
    }
}

// [[Rcpp::export]]
void PrintBoard(const Rcpp::IntegerMatrix& board)
{
    std::cout << board << std::endl;
}

// -------------------------------------------------------------- //
// File scope contants 
// -------------------------------------------------------------- //

static const unsigned int gol_board_nrow = 20;
static const unsigned int gol_board_ncol = 20;
static const unsigned int num_deltas     = 5;

// -------------------------------------------------------------- //
// board related
// -------------------------------------------------------------- //

// [[Rcpp::export]]
Rcpp::IntegerMatrix GetEvolvedBoard(Rcpp::IntegerMatrix& input_board)
{
    static const int nrow = gol_board_nrow;
    static const int ncol = gol_board_nrow;

    Rcpp::IntegerMatrix evolved_board(clone(input_board)); 

    // caculate the sourounding:
    for (unsigned int i = 0; i != nrow; i++)
    {
        for (unsigned int j = 0; j != ncol; j++)
        {
            unsigned int num_surrounding = 0;
            const unsigned int k_min = (i==0 ? 0 : i-1);
            const unsigned int k_max = (i==nrow-1 ? nrow-1 : i+1);
            const unsigned int l_min = (j==0 ? 0 : j-1);
            const unsigned int l_max = (j==ncol-1 ? ncol-1 : j+1);
            for (unsigned int k = k_min; k <= k_max; k++)
            {
                for (unsigned int l = l_min; l <= l_max; l++)
                {
                    bool is_center = (k == i && l == j);
                    if (not is_center) (num_surrounding += input_board(k, l)); 
                }
            }
    
            // update element
            evolved_board(i,j) = ((num_surrounding  == 3) || (num_surrounding == 2 && input_board(i, j)));
        }
    }

    return evolved_board;
}

// [[Rcpp::export]]
unsigned int GetNumAlive(const Rcpp::IntegerMatrix& board)
{   
   return std::accumulate(board.begin(), board.end(), 0.0); 
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix CreateStartBoard()
{
    using namespace Rcpp;

    // initalize the start board
    unsigned int nrow = gol_board_nrow;
    unsigned int ncol = gol_board_ncol;
    IntegerMatrix start_board(nrow, ncol);

    // loop until you get a board that isn't all dead
    unsigned int count_alive_cells = 0;
    while(count_alive_cells == 0)
    {
        // compute board's initial probability of the cell's being on
        double u = R::runif(0.0, 1.0);

        // fill cells of start board
        IntegerVector rv = as<IntegerVector>(rbinom(nrow*ncol, 1, u));
        rv.attr("dim") = Dimension(nrow, ncol);
        Rcpp::IntegerMatrix fresh_board = as<IntegerMatrix>(rv);

        count_alive_cells = GetNumAlive(fresh_board);
        if( count_alive_cells == 0)
        {
            //std::cout << "Dead before evolve" << std::endl;
            continue;
        }

        // evolve board
        start_board = fresh_board;
        for (int delta =  0; delta != 5; delta++)
        {
            start_board = GetEvolvedBoard(start_board);
        }

        // check the evoled board is not all dead
        count_alive_cells = GetNumAlive(start_board);
        if( count_alive_cells == 0)
        {
            //std::cout << "Dead after evolve" << std::endl;
        }
    } // end while loop

    return start_board;
}

// get tag given the board and the element
template <unsigned short sub_board_width>
unsigned int GetTag(const Rcpp::IntegerMatrix& board, const int i, const int j) 
{
    // static constants
    const int nrow = board.nrow();
    const int ncol = board.ncol();

    // offset (assumes odd) -- integer divide intentional
    static const int offset = (sub_board_width/2);

    // data structure to hold the subboard's bits
    std::bitset<sub_board_width*sub_board_width> bits;
    unsigned int bit_index = (sub_board_width*sub_board_width)-1;
    
    const int k_start = i - offset;
    const int k_end   = i + offset;
    const int l_start = j - offset;
    const int l_end   = j + offset;

    // fill the bitset
    for (int k = k_start; k <= k_end; k++)
    {
        //std::cout << "k = " << k << std::endl;
        for (int l = l_start; l <= l_end; l++)  
        {
            // check row boundaries
            if (k < 0 or k > (nrow-1))
            {
                bits.set(bit_index, false);
            }
            // check col boundaries
            else if (l < 0 or l > (ncol-1))
            {
                bits.set(bit_index, false);
            }
            else
            {
                bits.set(bit_index, static_cast<bool>(board(k,l)));
            }
            
            //std::cout << "\nbits(" << bit_index << ") = " << bits[bit_index] << std::endl;
            bit_index--;
        }
    }
    
    return static_cast<unsigned int>(bits.to_ulong());
}

// get tag bit string given the board and the element
template <unsigned short sub_board_width>
Rcpp::String GetTagString(const Rcpp::IntegerMatrix& board, const unsigned int i, const unsigned int j) 
{
   unsigned int tag = GetTag<sub_board_width>(board, i, j);
   return Rcpp::String(std::bitset<sub_board_width*sub_board_width>(tag).to_string());
}

// Tag manimulators
// [[Rcpp::export]]
unsigned int GetReversedTag( const unsigned int& input_tag)
{
    const unsigned int sub_board_width = 3;
    std::bitset<sub_board_width*sub_board_width>  bits(input_tag);
    std::bitset<sub_board_width*sub_board_width>  output_tag;
    for(unsigned int i = 0; i != output_tag.size(); i++)
    {
        output_tag[i] = bits[8-i];
    }
    return static_cast<unsigned int>(output_tag.to_ulong());
}

unsigned int GetFlippedTag( const unsigned int& input_tag)
{
    const unsigned int sub_board_width = 3;
    std::bitset<sub_board_width*sub_board_width>  bits(input_tag);
    std::bitset<sub_board_width*sub_board_width>  output_tag;
    for(unsigned int i = 0; i != output_tag.size(); i++)
    {
        output_tag[i] = bits[8-i];
    }
    std::cout << "\n" << output_tag << std::endl;
    
    return static_cast<unsigned int>(output_tag.to_ulong());
}


// [[Rcpp::export]]
unsigned int GetTag5by5(const Rcpp::IntegerMatrix& board, const unsigned int i, const unsigned int j) 
{
   return GetTag<5>(board, i, j); 
}

// [[Rcpp::export]]
unsigned int GetTag3by3(const Rcpp::IntegerMatrix& board, const unsigned int i, const unsigned int j) 
{
   return GetTag<3>(board, i, j); 
}

// [[Rcpp::export]]
Rcpp::String GetTagString5by5(const Rcpp::IntegerMatrix& board, const unsigned int i, const unsigned int j) 
{
   return GetTagString<5>(board, i, j); 
}

// [[Rcpp::export]]
Rcpp::String GetTagString3by3(const Rcpp::IntegerMatrix& board, const unsigned int i, const unsigned int j) 
{
   return GetTagString<3>(board, i, j); 
}


// -------------------------------------------------------------- //
// build the probability distributions 
// -------------------------------------------------------------- //

template < unsigned short sub_board_width> 
Rcpp::List BuildDistribution(const unsigned int num_boards, const unsigned int num_cutoff = 5)
{
    using namespace Rcpp;
    const unsigned int nrow = gol_board_nrow; 
    const unsigned int ncol = gol_board_ncol; 

    // define counts/alive vectors
    static const unsigned int num_tags = pow(2,sub_board_width*sub_board_width); 
    NumericMatrix num_counts(num_tags, num_deltas);
    NumericMatrix num_alive (num_tags, num_deltas);
    num_counts.fill(0);
    num_alive.fill(0);

    // loop over boards 
    unsigned int n = 0;
    while (n != num_boards-1)
    {
        // check the progress
        Progress(n, num_boards);

        // start board 
        IntegerMatrix start_board = CreateStartBoard(); 

        // evolve board times 
        IntegerMatrix end_board(clone(start_board)); 

        // loop over delta
        bool board_alive =false;
        for (unsigned int delta = 0; delta != num_deltas; delta++)
        {
            // evovle the board and check that
            // it's still alive --> go to next board 
            end_board = GetEvolvedBoard(end_board); 
            board_alive = (GetNumAlive(end_board) > 0);
            if (not board_alive) 
            { 
                break; 
            } 

            // extract counts for each tag 
            for (size_t i = 0; i != nrow; i++) 
            { 
                for (size_t j = 0; j != ncol; j++) 
                { 
                    unsigned int tag = GetTag<sub_board_width>(end_board, i, j); 
                    num_counts(tag, delta) = num_counts(tag,delta) + 1.0; 
                    if (start_board(i, j)==1)  
                    { 
                        num_alive(tag, delta) = num_alive(tag,delta) + 1.0; 

                    } 
                } // row loop 
            } // column loop 
        } // delta loop

        // only increment successfully evolved board
        if (board_alive)
        {
            n++;
        }
        else
        {
            continue;
        }
    } // end loop over boards 

    NumericMatrix probs(num_tags, num_deltas);
    for (unsigned int delta = 0; delta != num_deltas; delta++)
    {
        for (unsigned int tag = 0; tag != num_tags; tag++)
        {
            const unsigned int  counts = static_cast<unsigned int>(num_counts(tag, delta));
            if (counts == 0)
            {
                probs(tag, delta) = -1e-7;
            }
            if (0 < counts && counts <= num_cutoff)
            {
                probs(tag, delta) = -1*num_alive(tag, delta)/num_counts(tag, delta); 
            }
            else
            {
                probs(tag, delta) = num_alive(tag, delta)/num_counts(tag, delta);
            }
        }
    }

    //List prob_list = List::create
    //(
    //    Named("d1") = DataFrame::create(Named("prob")=probs(_, 0)),
    //    Named("d2") = DataFrame::create(Named("prob")=probs(_, 1)),
    //    Named("d3") = DataFrame::create(Named("prob")=probs(_, 2)),
    //    Named("d4") = DataFrame::create(Named("prob")=probs(_, 3)),
    //    Named("d5") = DataFrame::create(Named("prob")=probs(_, 4))
    //);

    List prob_list = List::create
    (
        Named("d1") = probs(_, 0),
        Named("d2") = probs(_, 1),
        Named("d3") = probs(_, 2),
        Named("d4") = probs(_, 3),
        Named("d5") = probs(_, 4)
    );
    return prob_list;
}

// [[Rcpp::export]]
Rcpp::List BuildDistribution5by5(const unsigned int num_boards = 1e3)
{
    return BuildDistribution<5>(num_boards);
}

// [[Rcpp::export]]
Rcpp::List BuildDistribution3by3(const unsigned int num_boards = 1e3)
{
    return BuildDistribution<3>(num_boards);
}

// -------------------------------------------------------------- //
// prediction
// -------------------------------------------------------------- //

// [[Rcpp::export]]
Rcpp::IntegerMatrix PredictBoardPlain
( 
    const Rcpp::IntegerMatrix& input_board, 
    const Rcpp::List& prob_dists, 
    const unsigned int delta = 1  
) 
{ 
    // ::::::::NOTE::::::
    // using R indexing conventions for delta
    
    using namespace Rcpp;
    const unsigned int nrow = gol_board_nrow; 
    const unsigned int ncol = gol_board_ncol; 
    Rcpp::String delta_index = "d" + Rcpp::toString(delta);
    const NumericVector& prob_5by5 = prob_dists[delta_index];

    if( prob_dists.size() != 5) 
    { 
        Rcpp::stop("[PredictBoard] prob_dist list must be size 5"); 
    } 

    // prod dists for a given delta
    //const Rcpp::NumericVector probs_5by5 = Rcpp::as<Rcpp::NumericVector>(prob_dists[delta-1           ]); 
    //const Rcpp::NumericVector probs_3by3 = Rcpp::as<Rcpp::NumericVector>(prob_dists[delta-1+num_deltas]); 

    Rcpp::IntegerMatrix prediction(nrow, ncol); 

    for( int i = 0; i < nrow; i++) 
    { 
        for( int j = 0; j < ncol; j++) 
        { 
            const unsigned int tag5 = GetTag<5>(input_board, i,j); 
            double prob = prob_5by5[tag5];
            if( prob < 0 )
            {
                prob = -1*prob;
            }
            prediction(i,j) = (prob > 0.5? 1 : 0); 
        } 
    } 
    return prediction; 
} 

// [[Rcpp::export]]
Rcpp::NumericMatrix PredictBoardProbs
( 
    const Rcpp::IntegerMatrix& input_board, 
    const Rcpp::List& prob_dists, 
    const unsigned int delta = 1  
) 
{ 
    // ::::::::NOTE::::::
    // using R indexing conventions for delta
     
    using namespace Rcpp;
    unsigned int num_NAs  = 0;
    const unsigned int nrow = gol_board_nrow; 
    const unsigned int ncol = gol_board_ncol; 
    Rcpp::String delta_index = "d" + Rcpp::toString(delta);
    const NumericVector& prob_5by5 = prob_dists[delta_index];

    if( prob_dists.size() != 5) 
    { 
        Rcpp::stop("[PredictBoard] prob_dist list must be size 5"); 
    } 

    Rcpp::NumericMatrix prediction(nrow, ncol); 

    for( int i = 0; i < nrow; i++) 
    { 
        for( int j = 0; j < ncol; j++) 
        { 
            const unsigned int tag5 = GetTag<5>(input_board, i,j); 
            double prob = prob_5by5[tag5];
            if( prob < 0 )
            {
                prob = -1*prob;
            }
            prediction(i,j) = prob; 
            // //bool check_na = Rcpp::is_na( prob );
            // if( R_IsNA(prob))
            // {
            //     std::cout << "NA converted to zero" << std::endl; 
            //     prediction = 0.0;
            //     num_NAs++;
            // }
        } // end col loop
    } // end row loop 

    std::cout << "\n number of NA's = " << num_NAs << std::endl;
    return prediction; 
} 


// [[Rcpp::export]]
Rcpp::IntegerMatrix PredictBoardCombine
( 
    const Rcpp::IntegerMatrix& input_board, 
    const Rcpp::List& prob_dists_5by5, 
    const Rcpp::List& prob_dists_3by3, 
    const unsigned int delta = 1  
) 
{ 
    // ::::::::NOTE::::::
    // using R indexing conventions for delta
    
    using namespace Rcpp;
    const unsigned int nrow = gol_board_nrow; 
    const unsigned int ncol = gol_board_ncol; 

    // get correct distributions
    Rcpp::String delta_index = "d" + Rcpp::toString(delta);
    const NumericVector& prob_5by5 = prob_dists_5by5[delta_index];
    const NumericVector& prob_3by3 = prob_dists_3by3[delta_index];

    bool bad_list = (prob_dists_5by5.size() != 5 || prob_dists_3by3.size() !=5);
    if(bad_list) 
    { 
        Rcpp::stop("[PredictBoard] prob_dist list must be size 5"); 
    } 

    // prod dists for a given delta
    //const Rcpp::NumericVector probs_5by5 = Rcpp::as<Rcpp::NumericVector>(prob_dists[delta-1           ]); 
    //const Rcpp::NumericVector probs_3by3 = Rcpp::as<Rcpp::NumericVector>(prob_dists[delta-1+num_deltas]); 

    Rcpp::IntegerMatrix prediction(nrow, ncol); 

    for( int i = 0; i < nrow; i++) 
    { 
        for( int j = 0; j < ncol; j++) 
        { 
            const unsigned int tag5 = GetTag<5>(input_board, i,j); 
            double prob = prob_5by5[tag5];
            if( prob < 0 && prob > -.9)
            {
                const unsigned int tag3 = GetTag<3>(input_board, i,j); 
                double prob = prob_3by3[tag3];
            }
            prediction(i,j) = (prob > 0.5? 1 : 0); 
        } 
    } 
    return prediction; 
} 

// -------------------------------------------------------------- //
// testing 
// -------------------------------------------------------------- //

#ifdef STANDALONE
// doesn't work --> compiles but crashes
// clang++ -g -O0 -std=c++11 -I/usr/local/Cellar/r/3.0.2/R.framework/Resources/include -DSTANDALONE -DNDEBUG  -I/usr/local/include -I/usr/local/Cellar/boost/1.55.0  -I"/usr/local/Cellar/r/3.0.2/R.framework/Versions/3.0/Resources/library/Rcpp/include" /usr/local/Cellar/r/3.0.2/R.framework/Versions/3.0/Resources/library/Rcpp/libs/Rcpp.so /usr/local/Cellar/r/3.0.2/R.framework/Libraries/lib*.dylib gol_R.cpp -o gol_R_test 
int main()
{
    Rcpp::List x1 = BuildDistribution5by5(1);
    return 0;
}

#endif
