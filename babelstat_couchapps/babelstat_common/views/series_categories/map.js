function(doc){
    if(doc.type){
	emit([doc.category, doc.sub_category, doc.subject],doc.series_category);
    }
}