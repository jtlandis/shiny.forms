

updateShinyFormColumn <- function(id, width = 6L, session = getDefaultReactiveDomain()){
  ns <- session$ns
  m <- list(
    id = ns(id),
    width = width
  )
  validate(need(is.integer(width), "`width` must be an Integer"))
  session$sendCustomMessage("updateShinyFormColumn", m)
}

# Special function used to correctly namespace and add proper handlers to
# the webpage. Most of these are to communicate clicks to the shiny session
# that are within a ShinyFormBuilder's UI elements.
get_ShinyForm_Element <- function(id, ns = NULL){
  selected <- "ShinyForm_selected_id"
  dom <- "ShinyForm_ele_ordered"
  children <- "ShinyForm_children"
  save <- "ShinyForm_save"
  if(!is.null(ns)){
    id <- ns(id)
    selected <- ns(selected)
    dom <- ns(dom)
    children <- ns(children)
    save <- ns(save)
  }
  glue::glue(
    .open = "<", .close = ">", .sep = "\n",
    "
    Shiny.addCustomMessageHandler('orderElementIDs', orderElementIDs)
    var ShinyForm_save_index = 0;
    function orderElementIDs(message) {
      let ids = [];
      $(message.query).map((index,ele) =>{
        ids.push(ele.id);
        });
      Shiny.setInputValue('<dom>', ids, {priority: 'event'});
      if(message.save) {
        ShinyForm_save_index = ShinyForm_save_index + 1;
        Shiny.setInputValue('<save>', ShinyForm_save_index, {priority: 'event'});
      }
    }
    Shiny.addCustomMessageHandler('updateShinyFormColumn', updateShinyFormColumn);
    function updateShinyFormColumn(message) {
      let col = $('#' + message.id);
      if(col.length !=0){
        col = col[0];
        col.className = col.className.replace(/(col-..-)([0-9]+)/, '$1' + message.width);
      }
    }
    var ShinyForm_selected = null;
    Shiny.addCustomMessageHandler('unselectShinyForm', setShinyFormNull);
    function setShinyFormNull(message) {
      ShinyForm_selected = null;
      Shiny.setInputValue('<selected>', null, {priority: 'event'})
    }
    Shiny.addCustomMessageHandler('findSubElements', findSubElements)
    function findSubElements(message) {
      if(ShinyForm_selected==null) {
        Shiny.setInputValue('<children>', null, {priority: 'event'}) ;
        return ;
      }
      let ids = [];
      $('.ShinyForm-selected').find(message.query).map((index, ele) => {
        ids.push(ele.id);
        });
      console.log(ids);
      Shiny.setInputValue('<children>', ids, {priority: 'event'});
    }
    Shiny.addCustomMessageHandler('appendParentWithSelected', appendParentWithSelected);
    function appendParentWithSelected(message) {
      if(ShinyForm_selected==null) { return ; }
      let parent = $('#' + message.parent)[0];
      $(message.selected_query).map((index, ele) => {
        parent.appendChild(ele);
        });
    }
    function ShinyColumn(el){
      if (el.classList.contains('ShinyForm-Column')||el.classList.contains('ShinyForm-Preview-Container')) {
        return el;
      }
      while(el && el.parentNode) {
        el = el.parentNode;
        if(el.classList.contains('ShinyForm-Column')||el.classList.contains('ShinyForm-Preview-Container')) {
          return el;
        }
      }
      return null;
    }

    $('#<id>').on('click', function(e){ // clicked in the container we care about

      if(e.target.id.length == 0) { return } // if no id -- return
      if(e.target.id == '<id>') { return } // if target is same as container -- return
      let shinyCol = ShinyColumn(e.target); // store the closest ShinyForm-Column node
     // if (shinyCol.childElementCount != 0 && e.target.id != shinyCol.id) { //if what we clicked is not the same as the container
          if(ShinyForm_selected == null) { // if unselected
            ShinyForm_selected = e.target;
            ShinyForm_selected.classList.add('ShinyForm-selected');
            Shiny.setInputValue('<selected>', ShinyForm_selected.id, {priority: 'event'});
          } else { // if an element/column is already selected
            ShinyForm_selected.classList.remove('ShinyForm-selected');
            if(ShinyForm_selected.id == e.target.id) {
              ShinyForm_selected = null;
              Shiny.setInputValue('<selected>', null, {priority: 'event'}); // tell shiny we deselected
            } else {
              ShinyForm_selected = e.target;
              ShinyForm_selected.classList.add('ShinyForm-selected');
              Shiny.setInputValue('<selected>', ShinyForm_selected.id, {priority: 'event'});
            }
          }
          return ;
     // }

    })
    ")}

