    var tab = $(\'a[data-value="results-tab"]\').parent().addClass("disabled");

    $(function(){
    $(tab.parent()).on("click","li.disabled", function(e) {
      e.preventDefault();
      return false;
    });
    });

