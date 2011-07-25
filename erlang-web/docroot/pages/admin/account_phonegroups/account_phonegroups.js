//alert('lalal')

function get_acd_count()
{
	var count = 0;
	$('ul.connectedSortable').each(function(i,l) { count++ });
	return count
}

function edit_acd_rules(AcdID)
{
	goApp.currentAccountAcdId = AcdID;
	$("#example").dialog("open");
}

function acd_rules_dialog_acivate()
{
	//alert(goApp.currentAccountAcdId);
	create_acdrule_grid(goApp.currentAccountAcdId);
}



$(function(){
	$.post("/json/phone_number",
		{ request: JSON.stringify({
		   commands:[{
		      type:"select",
		      fields:["id", "number"],
		      orderBy:[{field:"id", order:"desc"}],
		      filter:{match:"and",rules:[
			{field:"account_id",match:"equal",value:0},
			{field:"organization_id",match:"equal",value:goApp.currentOrganizationId},
			{field:"active",match:"equal",value:"Y"}]},
		      needFields:false}]}) },
		function(data){
		  //alert("Data Loaded: " + data);
		  var obj = jQuery.parseJSON(data);
		  if(obj.commands[0].data){
			$.each(obj.commands[0].data, function(index, value){
				$('#sortable0').append($(
					'<li id="ph_'+value.id+
					'" class="ui-state-highlight">'+value.number+'</li>'))
			})
		  }
		}
	      );
	$('#b_create_acd, #b_save_acds, #b_cancel_acds').button();
	$('#b_create_acd').click(function() {
	   var NewSortableId = get_acd_count()+1;
	   $('#div_acds').append($('<ul id="sortable'+NewSortableId+'" class="connectedSortable"><li class="emptyMessage">ACD</li></ul>'));
	   $('#sortable'+NewSortableId).sortable(
		{'connectWith':'.connectedSortable',
		'dropOnEmpty':true,
		'scroll':true,
		items: "li:not(.emptyMessage)"
	   });
	   
	});
	$('#b_save_acds').click(function() {
		var acds_obj = new Array();
		$('ul.connectedSortable').each(function(i,l) {
		   if(l.id != 'sortable0'){
		      $('#'+l.id).children(".ui-state-highlight").each(function(ii,ll){
			$(ll).contents().each(function(iii, lll){
			   if(lll.nodeType == 3){
				var acd_id     = l.id.substring(8);
				var acd_obj    = new Object();
				acd_obj.acd_id = parseInt(acd_id);
				acd_obj.account_id = goApp.currentAccountId;
				acd_obj.number = lll.nodeValue;
				acds_obj.push(acd_obj)
			   }
			});
		      })
		   }
		});
		$.post("/json/account/acd",
		  { request: JSON.stringify({
		   commands:[{
		      type:"save",
		      acds: acds_obj,
		      account_id:goApp.currentAccountId
		      }]}) },
		  function(data){alert("SAVE response: " + data)}
		);
	});	
	// get users acd and data
	$.post("/json/account/acd",
		{ request: JSON.stringify({
		   commands:[{
		      type:"select",
		      account_id:goApp.currentAccountId}]}) },
		function(data){
		  //alert("Data Loaded: " + data);
		  var obj = jQuery.parseJSON(data);
		  if(obj.commands.data){
			$.each(obj.commands.data, function(index, value){
			   $('#div_acds').append($('<ul id="sortable'+value.acd_id+
				'" class="connectedSortable"><li class="emptyMessage" onclick="edit_acd_rules('+value.acd_id+')">ACD'+value.acd_id+'</li></ul>'));
			   $('#sortable'+value.acd_id).sortable(
				{'connectWith':'.connectedSortable',
				'dropOnEmpty':true,
				'scroll':true,
				items: "li:not(.emptyMessage)"
			   });
			   $.each(value.numbers, function(i, v){
				$('#sortable'+value.acd_id).append($(
					'<li id="phn_'+v+'" class="ui-state-highlight">'+v+'</li>'))
			   })
			   
			})
		  }
		}
	      );
});

$(function(){
	$('#sortable0')
	    .sortable(
		{'connectWith':'.connectedSortable',
		 'dropOnEmpty':true,
		 'scroll':true,
		 items: "li:not(.emptyMessage)",
		 receive: function(event, ui) {
			 //hide empty message on receiver
			 /*$('li.emptyMessage', this).hide();
			 
			 //show empty message on sender if applicable
			 if($('li:not(.emptyMessage)', ui.sender).length == 0){
			     $('li.emptyMessage', ui.sender).show();
			 } else {
			     $('li.emptyMessage', ui.sender).hide();
			 }
			 */
		     }
		 
		});
	    
	var dialogOpts = {
		modal: true,
		bgiframe: true,
		autoOpen: false,
		height: 500,
		width: 700,
		draggable: true,
		resizeable: true,
		title: "Acd Rules",
		open: function() {
		//display correct dialog content
		$("#example").load("/admin/acdrules/"+goApp.currentAccountId+"/"+goApp.currentAccountAcdId+"/",
			function(){ acd_rules_dialog_acivate(); } )}
		};
	$("#example").dialog(dialogOpts);	//end dialog
	
	$("#accordion").accordion({ clearStyle: true });
});


function create_acdrule_grid(rule_id){
	jQuery("#list6").jqGrid({        
		url:'/json/accounts/acd/rules/'+rule_id,
		datatype: "json",
		colNames:['ID', 'Priority', 'Name', 'Time Period', 'Active'],
		colModel:[
			{name:'id',index:'id', width:50},
			{name:'priority',index:'priority', width:100, editable:true, editrules:{required:true, integer:true}},
			{name:'name',index:'forward_type', sortable:false, width:230, editable:true, editrules:{required:true}},
			{name:'time_period',index:'number', sortable:false, width:230, align:"right", editable:true, edittype:"select", editoptions:{ value: "* * * * *:Always" },editrules:{required:true}},
			{name:'active',index:'active', sortable:false, width:60,align:"right", editable:true, edittype:"checkbox", editrules:{required:true}}		
		],
		rowNum:10,
		rowList:[10,20,30],
		pager: '#pager6',
		sortname: 'id',
		viewrecords: true,
		sortorder: "desc",
		onSelectRow: function(ids) {
			if(ids == null) {
				ids=0;
				if(jQuery("#list10_d").jqGrid('getGridParam','records') >0 )
				{
					jQuery("#list10_d").jqGrid('setGridParam',
					   {url:"/json/accounts/acd/rulenumbers/"+rule_id+"/"+ids,page:1,
					    editurl:  "/json/accounts/acd/rulenumbers/"+rule_id+"/"+ids+"/update"});
					jQuery("#list10_d").jqGrid('setCaption',"Acd Numbers: "+ids)
					.trigger('reloadGrid');
				}
			} else {
				jQuery("#list10_d").jqGrid('setGridParam',
				   {url:"/json/accounts/acd/rulenumbers/"+rule_id+"/"+ids,page:1,
				    editurl:  "/json/accounts/acd/rulenumbers/"+rule_id+"/"+ids+"/update"});
				jQuery("#list10_d").jqGrid('setCaption',"Acd Numbers: "+ids)
				.trigger('reloadGrid');			
			}
		},
		height: 150,
		ondblClickRow: function(rowid){
			jQuery(this).jqGrid('editGridRow', rowid, {
				recreateForm:true,closeAfterEdit:true,
				closeOnEscape:true, zIndex:1234});
		}
	});
	jQuery("#list6").jqGrid('navGrid',"#pager6",{edit:true,add:true,del:true},
		{
			zIndex:1234,
			closeAfterEdit:true,
			closeOnEscape:true,
			url: '/json/accounts/acd/rules/'+rule_id+'/update'
		},
		{
			closeAfterAdd:true,
			saveData: "Data has been changed! Save changes?",
			zIndex:1234,
			url: '/json/accounts/acd/rules/'+rule_id+'/add',
			closeOnEscape:true
		},
		{
			closeOnEscape:true,
			zIndex:1234,
			url: '/json/accounts/acd/rules/'+rule_id+'/delete'
		},
		{},{});
	
	jQuery("#list10_d").jqGrid({
		height: 150,
		datatype: "json",
		colNames:['ID','Order', 'Forward type', 'Number', 'TimeOut','Active'],
		colModel:[
			{name:'id',index:'id', width:80},
			{name:'order',index:'order', width:80, editable:true, editrules:{required:true, integer:true}},
			{name:'forward_type',index:'forward_type', sortable:false, width:180, editable:true, editrules:{required:true}, edittype:"select", editoptions:{ value: "E:Extension;N:Phone Number;D:Device" }},
			{name:'number',index:'number', width:150, sortable:false, editable:true, editrules:{required:true, integer:true}},
			{name:'timeout',index:'timeout', width:100, sortable:false, editable:true, editrules:{required:true, integer:true}},
			{name:'active',index:'active', width:80, sortable:false, search:false, edittype:"checkbox", editable:true, editrules:{required:true}}
		],
		rowNum:5,
		rowList:[5,10,20],
		pager: '#pager10_d',
		sortname: 'item',
		viewrecords: true,
		sortorder: "asc",
		caption: "acd numbers",
		ondblClickRow: function(rowid){
			jQuery(this).jqGrid('editGridRow', rowid, {
				recreateForm:true,closeAfterEdit:true,
				closeOnEscape:true, zIndex:1234});
		}
	});
	jQuery("#list10_d").jqGrid('navGrid',"#pager10_d",{edit:true,add:true,del:true},
		{
			zIndex:1234,
			closeAfterEdit:true,
			closeOnEscape:true
		},
		{
			closeAfterAdd:true,
			saveData: "Data has been changed! Save changes?",
			zIndex:1234,
			closeOnEscape:true
		},
		{
			closeOnEscape:true,
			zIndex:1234
		},
		{},{});

}


