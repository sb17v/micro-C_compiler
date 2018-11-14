#include "global.h"

/* TO BE COMPLETED: implement backpatch operations on lists */
Backpatchlist makelist(int location){
    Backpatchlist  new_backpatchlist;
    new_backpatchlist.backpatch_array[0] = location;
    new_backpatchlist.last_entry = 1;
    return new_backpatchlist;
}

void backpatchlist(Backpatchlist list, int location){
    int last_entry =  list.last_entry;
    int prev_location;
    for(int i = 0; i < last_entry && i<125; i++){
        
        prev_location = list.backpatch_array[i];
        backpatch(prev_location, location - prev_location);
    }
}

Backpatchlist mergelist(Backpatchlist list1, Backpatchlist list2){
    int i;
    int j;
    Backpatchlist final_list = list1;
    if(list1.last_entry+list2.last_entry >= 125){
        error("Cannot Backpatch");
        return final_list;
    }
    for (i = final_list.last_entry, j = 0; j < list2.last_entry; i++, j++)
	{		
		final_list.backpatch_array[i] = list2.backpatch_array[j];
	}
	final_list.last_entry = i;
	return final_list;
}

void print_backpatchlist(Backpatchlist list){
    printf("Printing Backpatch----------\n");
    for(int i=0; i < list.last_entry; i++){
        printf("Backpatchlist[%d] = %d\n", i, list.backpatch_array[i]);
    }
}