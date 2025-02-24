export function toggleSidebar() {
  let sidebar = document.getElementById('sidebar');
  let buttonContainer = document.querySelector('.button-container');
  if (sidebar.classList.contains('active')) {
      sidebar.classList.remove('active');
      buttonContainer.classList.remove('moved');
  } else {
      sidebar.classList.add('active');
      buttonContainer.classList.add('moved')      
  }
}

export function activeRecreation() {
  let recre = document.getElementById("slidersSidebar")
  recre.classList.remove("d-none")
  recre.classList.add("d-block")
  let species = document.getElementById("speciesSidebar")
  species.classList.add("d-none")
  let maps = document.getElementById("mapsSidebar")
  maps.classList.add("d-none")
}

export function deactRecreation() {
  let recre = document.getElementById("slidersSidebar")
  recre.classList.add("d-none")
}

export function activeSpecies() {
  let species = document.getElementById("speciesSidebar")
  species.classList.remove("d-none")
  species.classList.add("d-block")
  let recre = document.getElementById("slidersSidebar")
  recre.classList.add("d-none")
  let maps = document.getElementById("mapsSidebar")
  maps.classList.add("d-none")
}

export function deactSpecies() {
  let species = document.getElementById("speciesSidebar")
  species.classList.add("d-none")
}

export function activeMaps() {
  let maps = document.getElementById("mapsSidebar")
  maps.classList.remove("d-none")
  maps.classList.add("d-block")
  let recre = document.getElementById("slidersSidebar")
  recre.classList.add("d-none")
  let species = document.getElementById("speciesSidebar")
  species.classList.add("d-none")
}

export function deactMaps() {
  let maps = document.getElementById("mapsSidebar")
  maps.classList.add("d-none")
}
