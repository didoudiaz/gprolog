#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int methods_tested = 0;
int tests_run = 0;
char *last_test = "-";
char *test_message = "-";

/* Altered version of minunit unit tests */
#define test_constr(constr, exp_bounds)					\
	do {												\
		tests_run++; 									\
		test_message = check_test(constr,exp_bounds);	\
		if (test_message != NULL) 						\
			return constr;								\
	} while (0)
	

#define run_test(test, testname) 	\
	do { 							\
		char *message = test(); 	\
		methods_tested++; 			\
		last_test = testname;		\
		if (message) 				\
			return message; 		\
	} while (0)

/* Helper method for check_test */
int startsWith(const char *str, const char *pre) {
    size_t lenpre = strlen(pre),
           lenstr = strlen(str);
    return lenstr < lenpre ? 0 : strncmp(pre, str, lenpre) == 0;
}

/* Runs the interpreter with the written input, and tests it with the exp_output */
char * check_test(char *constr, char *exp_output) {
	/* Writes an input file with the given constraint */
	FILE *f = fopen("input.txt", "w");
	if (*(constr + strlen(constr) - 1) != '.') 
  		fprintf(f,"%s.\n", constr);
  	else fprintf(f,"%s\n", constr);
	fclose(f);

	// run the interpreter with the written input and save this in output.txt
	system("gprolog < input.txt > output.txt\n");
	
	// variables
	static char output_buff[4096];
 	char *exp_words[128];
	char line[4096];
	int test_ok = 1;
	int found_FDvars = 0;
	int nn_e, n_e=0;
	int nn_o, n_o = 0;
	char *ds_e;
 	char *output_words[128];
 	const char DELIM[] = " =[],()|\n";

	// parse exp_output to words
	ds_e=strdup(exp_output);
	exp_words[n_e] = strtok(ds_e, DELIM);
	while (exp_words[n_e] && n_e<128) exp_words[++n_e] = strtok(NULL, DELIM);
	if (n_e==0) {
		sprintf(output_buff, "Cannot parse expected output: \"%s\"\n", exp_output);
		free(ds_e);
		return output_buff;
	} else if (!startsWith(exp_words[0],"no"))
		exp_words[n_e++] = "yes";
	exp_words[n_e++] = "?-";

	// parse output
	f = fopen("output.txt", "rt"); 
	while(!found_FDvars && test_ok && fgets(line, 4096, f) != NULL) {
		if (startsWith(line,"| ?-"))
			found_FDvars = 1;
	}
	// parse output to words
	while (fgets(line, 4096, f) != NULL) {
		char *ds_o = strdup(line);
		output_words[n_o] = strtok(ds_o, DELIM);
		while (output_words[n_o] && n_o<128) {
			if (startsWith(output_words[n_o],"_"))
				output_words[n_o] = strtok(NULL, DELIM);
			output_words[++n_o] = strtok(NULL, DELIM);
		}
		// (x ms) yes  : (remove the first part)
		if (*line == '(') {
			output_words[n_o-3] = output_words[n_o-1];
			n_o -= 2;
		}
	} 

	// check the output
	for (nn_o=0; nn_o<n_o; nn_o++) {
		if (output_words[nn_o] == NULL) {
			return "Cannot parse output, please check if all debug output is turned off.";
		}
		//printf("[%s]\n", output_words[n_o]);
	}

	// compare the FDvars
	test_ok = (n_o == n_e);
	if (found_FDvars && test_ok) {
		nn_o = 0;
		for (nn_e=0; nn_e<n_e; nn_e++) {
			if (strcmp(exp_words[nn_e], output_words[nn_e]) != 0) 
				test_ok = 0;
		}
	}

   	fclose(f); 
	if (!test_ok) {
		// write the output to the buffer
		sprintf(output_buff + strlen(output_buff), "found:\n");
		for (nn_o = 0; nn_o+1<n_o; nn_o+= 2) {
			if (strcmp(output_words[nn_o], "no") == 0)
				sprintf(output_buff + strlen(output_buff), "  no\n");
			else if (strcmp(output_words[nn_o], "yes") != 0)
				sprintf(output_buff + strlen(output_buff), "  %s = (%s)\n", output_words[nn_o], output_words[nn_o+1]);
		}

		// write the expected output to the buffer
		sprintf(output_buff + strlen(output_buff), "expected:\n");
		for (nn_e = 0; nn_e+1<n_e; nn_e+= 2) {
			if (strcmp(exp_words[nn_e], "no") == 0)
				sprintf(output_buff + strlen(output_buff), "  no\n");
			else if (strcmp(exp_words[nn_e], "yes") != 0)
				sprintf(output_buff + strlen(output_buff), "  %s = (%s)\n", exp_words[nn_e], exp_words[nn_e+1]);
		}
		free(ds_e);
		return output_buff;
	}
	free(ds_e);
	return 0;
}



/*-------------------------------------------------------------------------*
 * test_eq_neq                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/

static char * test_eq_neq() {
	// EQ
	test_constr("X #= -268435456.",  "no");
	test_constr("X #= -268435455.",  "X = -268435455");
	test_constr("X #= -268435454.",  "X = -268435454");
	test_constr("X #= -5.",  "X = -5");
	test_constr("X #= 0.",  "X = 0");
	test_constr("X #= 5.",  "X = 5");
	test_constr("X #= 268435454.",  "X = 268435454");
	test_constr("X #= 268435455.",  "X = 268435455");
	test_constr("X #= 268435456.",  "no");

	test_constr("fd_domain(X,-100,100), X #= -101.",  "no");
	test_constr("fd_domain(X,-100,100), X #= -100.",  "X = -100");
	test_constr("fd_domain(X,-100,100), X #= -99.",  "X = -99");
	test_constr("fd_domain(X,-100,100), X #= -5.",  "X = -5");
	test_constr("fd_domain(X,-100,100), X #= 0.",  "X = 0");
	test_constr("fd_domain(X,-100,100), X #= 5.",  "X = 5");
	test_constr("fd_domain(X,-100,100), X #= 99.",  "X = 99");
	test_constr("fd_domain(X,-100,100), X #= 100.",  "X = 100");
	test_constr("fd_domain(X,-100,100), X #= 101.",  "no");

	// NEQ
	test_constr("X #\\= -268435456.",  "no");
	test_constr("X #\\= -268435455.",  "X = [-268435454..268435455]");
	test_constr("X #\\= -268435454.",  "X = [-268435455:-268435453..268435455]");
	test_constr("X #\\= -5.",  "X = [-268435455..-6:-4..268435455]");
	test_constr("X #\\= 0.",  "X = [-268435455..-1:1..268435455]");
	test_constr("X #\\= 5.",  "X = [-268435455..4:6..268435455]");
	test_constr("X #\\= 268435454.",  "X = [-268435455..268435453:268435455]");
	test_constr("X #\\= 268435455.",  "X = [-268435455..268435454]");
	test_constr("X #\\= 268435456.",  "no");

	test_constr("fd_domain(X,-100,100), X #\\= -101.",  "X = [-100..100]");
	test_constr("fd_domain(X,-100,100), X #\\= -100.",  "X = [-99..100]");
	test_constr("fd_domain(X,-100,100), X #\\= -99.",  "X = [-100:-98..100]");
	test_constr("fd_domain(X,-100,100), X #\\= -5.",  "X = [-100..-6:-4..100]");
	test_constr("fd_domain(X,-100,100), X #\\= 0.",  "X = [-100..-1:1..100]");
	test_constr("fd_domain(X,-100,100), X #\\= 5.",  "X = [-100..4:6..100]");
	test_constr("fd_domain(X,-100,100), X #\\= 99.",  "X = [-100..98:100]");
	test_constr("fd_domain(X,-100,100), X #\\= 100.",  "X = [-100..99]");
	test_constr("fd_domain(X,-100,100), X #\\= 101.",  "X = [-100..100]");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_gt_lt                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/

static char * test_gt_lt() {
	// GT
	test_constr("X #> -268435456.",  "X = [-268435455..268435455]");
	test_constr("X #> -268435455.",  "X = [-268435454..268435455]");
	test_constr("X #> -268435454.",  "X = [-268435453..268435455]");
	test_constr("X #> -5.",  "X = [-4..268435455]");
	test_constr("X #> 0.",  "X = [1..268435455]");
	test_constr("X #> 5.",  "X = [6..268435455]");
	test_constr("X #> 268435454.",  "X = [268435455]");
	test_constr("X #> 268435455.",  "no");
	test_constr("X #> 268435456.",  "no");

	// LT
	test_constr("X #< -268435456.",  "no");
	test_constr("X #< -268435455.",  "no");
	test_constr("X #< -268435454.",  "X = [-268435455]");
	test_constr("X #< -5.",  "X = [-268435455..-6]");
	test_constr("X #< 0.",  "X = [-268435455..-1]");
	test_constr("X #< 5.",  "X = [-268435455..4]");
	test_constr("X #< 268435454.",  "X = [-268435455..268435453]");
	test_constr("X #< 268435455.",  "X = -268435455..268435454");
	test_constr("X #< 268435456.",  "X = -268435455..268435455");

	// GTE
	//test_constr("X #>= -268435456.",  "X = [-268435455..268435455]");
	test_constr("X #>= -268435455.",  "X = [-268435455..268435455]");
	test_constr("X #>= -268435454.",  "X = [-268435454..268435455]");
	test_constr("X #>= -5.",  "X = [-5..268435455]");
	test_constr("X #>= 0.",  "X = [0..268435455]");
	test_constr("X #>= 5.",  "X = [5..268435455]");
	test_constr("X #>= 268435454.",  "X = [268435454..268435455]");
	test_constr("X #>= 268435455.",  "X = [268435455]");
	test_constr("X #>= 268435456.",  "no");

	// LTE
	test_constr("X #=< -268435456.",  "no");
	test_constr("X #=< -268435455.",  "X = [-268435455]");
	test_constr("X #=< -268435454.",  "X = [-268435455..-268435454]");
	test_constr("X #=< -5.",  "X = [-268435455..-5]");
	test_constr("X #=< 0.",  "X = [-268435455..0]");
	test_constr("X #=< 5.",  "X = [-268435455..5]");
	test_constr("X #=< 268435454.",  "X = [-268435455..268435454]");
	test_constr("X #=< 268435455.",  "X = [-268435455..268435455]");
	//test_constr("X #=< 268435456.",  "X = -268435455..268435455");

	return 0;
}

/*-------------------------------------------------------------------------*
 * test_x_plus_y                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/

static char * test_x_plus_y() {
	test_constr("X + Y #= Z.",  
		"X = [-268435455..268435455], Y = [-268435455..268435455], Z = [-268435455..268435455]");

	return 0;
}

/*-------------------------------------------------------------------------*
 * General Test-related Methods                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
 
static char * all_tests() {
	run_test(test_eq_neq,"test_eq_neq");
	run_test(test_gt_lt,"test_gt_lt");
	run_test(test_x_plus_y,"test_x_plus_y");

	return 0;
}


int main(int argc, char *argv[])
{
	printf("\n\n");
	char *result = all_tests();
	if (result != 0) {
		printf("\n\nTEST FAIL: %s\n\n%s\n\nIn %s\n\n", result, test_message, last_test);
	}
	else {
		printf("ALL TESTS PASSED\n");
	}
	printf("\n\n");
	printf("Methods tested:  %d\n", methods_tested);
	printf("Total Tests run: %d\n", tests_run);

	// cleanup
	system("rm input.txt\n");
	system("rm output.txt\n");

	return result != 0;
}
