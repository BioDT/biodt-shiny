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
  console.log(recre)
  recre.classList.remove("d-none")
  recre.classList.add("d-block")
  let species = document.getElementById("speciesSidebar")
  species.classList.add("d-none")
}

export function deactRecreation() {
  let recre = document.getElementById("slidersSidebar")
  recre.classList.add("d-none")
  console.log(recre)
}

export function activeSpecies() {
  let species = document.getElementById("speciesSidebar")
  console.log(species)
  species.classList.remove("d-none")
  species.classList.add("d-block")
  let recre = document.getElementById("slidersSidebar")
  recre.classList.add("d-none")
}

export function deactSpecies() {
  let species = document.getElementById("speciesSidebar")
  species.classList.add("d-none")
  console.log(species)
}