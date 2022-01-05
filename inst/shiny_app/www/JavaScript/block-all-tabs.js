
var tab = $('a[data-value="preprocessing-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="results-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="heatmap-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="comparisons-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="volcano-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="profile-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="enrichment-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="disease-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="network-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="pathway-tab"]').parent().addClass("disabled");
var tab = $('a[data-value="interactions-tab"]').parent().addClass("disabled");

$(function(){
  $(tab.parent()).on("click","li.disabled", function(e) {
    e.preventDefault();
    return false;
  });
});


