function(doc){
    if(doc.type){
	emit(doc.category,1);
    }
}