function(doc){   
    if(doc.type){
	emit([doc.category,doc.subcategory,doc.subject, doc.series_category, doc.title, doc.date],[]);
    }
}