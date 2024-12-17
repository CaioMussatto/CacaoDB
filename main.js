function openPage(evt, pageName) {
    var i, tabcontent, tablinks;
    tabcontent = document.getElementsByClassName("tabcontent");
    for (i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = "none";
    }
    tablinks = document.getElementsByClassName("tablinks");
    for (i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(" active", "");
    }
    document.getElementById(pageName).style.display = "block";
    evt.currentTarget.className += " active";
  }
  
  function getTissue() {
    return sessionStorage.getItem('SC_tissue');
}

  function setTissue(tissue='') {
    sessionStorage.setItem('SC_tissue',tissue);
    document.getElementById('tissue_sc').setAttribute('value', tissue);
    document.getElementById('tissue_sc').setAttribute('data-shinyjs-resettable-value', tissue);
    
   
}

function setOrganism(organism=''){
   sessionStorage.setItem('SC_organism', organism)
     document.getElementById('organism_sc').setAttribute('value', organism);
     document.getElementById('organism_sc').setAttribute('data-shinyjs-resettable-value', organism);
 
}
  
function getOrganism(){
  return sessionStorage.getItem('SC_organism')
}

 function stopLoader(){
  document.getElementById('loader').add('hidden');
  alert('stopping ?');
}
  