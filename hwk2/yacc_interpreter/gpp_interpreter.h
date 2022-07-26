#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>

// int pow_gpp(int n, int exp);//DBL_MULT
// 	int getIdentifier(char* name);//getting value of an identifier
// 	void set_element( char* name, int value );//appending
// 	int* append_list(int* list, int num);
// 	void freeList();
// 	int* concatenate_lists(int* list1, int* list2);
// 	void print_list(int* list);

typedef struct node{
	int value;
	char identifier[12];
	struct node* next;
}node;

node* list_symbol_table=NULL;

void print_list(int* list_formed ){
	if( list_formed  == NULL ){
		printf("nil");
		return;
	}
	printf("( ");
	for( int i = 0; list_formed[i] != INT_MAX; ++i ) printf("%d ", list_formed[i]);
	printf(")\n");
	return;
}
int pow_gpp_help(int n, int exp){
	if( exp == 0 )
		return 1;
	if( exp == 1 )
		return n;
	return n*pow_gpp_help(n, exp-1);
}

int pow_gpp(int n, int exp){
	int res;
	if( exp<0 ){
		return 0;
	}
	else{
		res = pow_gpp_help(n, exp);
	}
	return res;
}


void print_symbol_table(node* temp){
	if( temp == NULL )
		return;
	// printf("ID: %s VALUE: %d\n", temp ->identifier, temp->value);	
	print_symbol_table(temp->next);
}
//append here
void set_element( char* name, int value ){

	// printf("IN SET ELEMENT\n");
	if( list_symbol_table == NULL ){
		list_symbol_table = (node*)malloc(sizeof(node));
		strcpy(list_symbol_table->identifier, name);
		list_symbol_table->value = value;
		list_symbol_table->next = NULL;
	}
	else{
		node* temp = list_symbol_table;
		while( temp->next != NULL && (strcmp(temp->identifier, name) != 0) ) temp = temp->next;
		//update the value or add a new value!
		//check if adding a new value is necessary
		if( strcmp( temp->identifier, name )==0 ){
			//update value if this is the case
			temp->value = value;
		}else{
			// printf("IN HERE TEMP IS AT: %s - NEW NAME IS %s\n", temp->identifier, name);
			//otherwise add a new value / identifier		
			node* new_node = (node*)malloc(sizeof(node));
			new_node->value = value;
			strcpy(new_node->identifier, name);
			new_node->next = NULL;
			//update the symbol list here
			temp -> next = new_node;
			//print_symbol_table(list_symbol_table);
		}
	}
}

//get the identifier
int getIdentifier(char* name){
	node* temp = list_symbol_table;
	while( temp != NULL ){
		if( strcmp(name, temp->identifier) == 0 )
			return temp->value;
		temp = temp -> next;
	}
	printf("%s does not exist\n", name);
	return 0;
}


// //compare list_symbol_tables here for example
// int compare_list_symbol_tables(int* arr1, int* arr2){

// }
//if here, equal compare etc
int compare_list(node* list_symbol_table1, node* list_symbol_table2 ){
	while( list_symbol_table1 != NULL && list_symbol_table2 != NULL ){
		if( (list_symbol_table1 -> value != list_symbol_table2 -> value) || ( strcmp(list_symbol_table1->identifier, list_symbol_table2->identifier) != 0) )
			return 0;
		list_symbol_table1 = list_symbol_table1->next;
		list_symbol_table2 = list_symbol_table2->next;
	}

	return list_symbol_table1==list_symbol_table2;
}
//concatenate here
int* concatenate_lists(int* list_symbol_table1, int* list_symbol_table2){
	int* l1 = list_symbol_table1;
	int* l2 = list_symbol_table2;

	int n1=0;
	int n2=0;


	if( l1 == NULL )
		return l2;
	if( l2 == NULL )
		return l1;


	while( l1[++n1] != INT_MAX );
	while( l2[++n2] != INT_MAX );
	// printf("%d %d", n1, n2);
	int* list_symbol_table_ret = (int*)malloc(sizeof(int)*(n1+n2+1));
	for(int i =0; i<n1; ++i )
		list_symbol_table_ret[i] = l1[i];
	for(int i =n1; i<n1+n2; ++i )
		list_symbol_table_ret[i] = l2[i-n1];

	list_symbol_table_ret[n1+n2] = INT_MAX;
	

	return list_symbol_table_ret;
}	
//append list_symbol_tables
int* append_list(int* formed_list, int num){
	//for expression and everything...
	if( formed_list == NULL ){
		formed_list = (int*)malloc(sizeof(int)*2);
		formed_list[0] = num;
		formed_list[1] = INT_MAX;
		return formed_list;
	}

	//add a new element now to the previous element
	int count=0;
	while( formed_list[++count] != INT_MAX );

	int* new_list = (int*)malloc(sizeof(int)*count+2);
	for(int i=0; i<count; ++i )
		new_list[i] = formed_list[i];

	free(formed_list);
	new_list[count] = num;
	new_list[count+1] = INT_MAX;

	return new_list;
}
// void main(){

// 	set_element("x", 5);
// 	set_element("y", 21);
// 	set_element("z", 40);
	
// 	printf("%d", pow_gpp(2, -5));
// }

void freeList(){
	while( list_symbol_table != NULL ){
		node* to_free = list_symbol_table;
		list_symbol_table = list_symbol_table -> next;
		free(to_free);
	}
}