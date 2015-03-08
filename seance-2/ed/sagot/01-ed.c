enum instruction
{
	I, D, R, E
} ;

void read_instruction (enum instruction * i)
{
}

void interpret_instruction (enum instruction i)
{
}

void editor (char * x, char * y, int input, int output)
{
	enum instruction i ;

	do
	{
		read_instruction ( &i ) ;
		interpret_instruction ( i ) ;
	}
	while (i != E) ;
}


int main (void)
{
	editor ((char *)0, (char *)0, 0, 0) ;
	return 0;
}
