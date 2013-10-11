#include <stdio.h>

int new_io_reader(int ftype, char * inFile, char * configFile){
  printf("\nnew_io_reader:\n");
  printf("     ftype:%d\n",ftype);
  printf("    inFile:%s\n",inFile);
  printf("configFile:%s\n",configFile);

  return 0;
}

int get_dimensions(int * io,int * sb, char * varName){
  printf("\nGetting dimensions for varName: %s \n",varName);
  // new_slicebuilder
  // sb_get_start_size_pos
  return 0;
}

int set_dimensions(int * sb){

  // sb_set_start_size
  return 0;
}

int read_field(int * io,int * sb, char * units, double * data, int * dataRead){

  // io_get_sb_data_double
  return 0;
}

int close_file(int * io, int * sb){

  // free_slicebuilder
  // free_io_reader
  return 0;
}
