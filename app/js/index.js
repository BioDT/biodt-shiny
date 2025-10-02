export function toggleSidebar() {
  const sidebar = document.getElementById('sidebar');
  const buttonContainer = document.querySelector('.button-container');
  if (sidebar.classList.contains('active')) {
    sidebar.classList.remove('active');
    buttonContainer.classList.remove('moved');
  } else {
    sidebar.classList.add('active');
    buttonContainer.classList.add('moved');
  }
}

export function activeRecreation() {
  const recre = document.getElementById('slidersSidebar');
  recre.classList.remove('d-none');
  recre.classList.add('d-block');
  const species = document.getElementById('speciesSidebar');
  species.classList.add('d-none');
  const maps = document.getElementById('mapsSidebar');
  maps.classList.add('d-none');
}

export function deactRecreation() {
  const recre = document.getElementById('slidersSidebar');
  recre.classList.add('d-none');
}

export function activeSpecies() {
  const species = document.getElementById('speciesSidebar');
  species.classList.remove('d-none');
  species.classList.add('d-block');
  const recre = document.getElementById('slidersSidebar');
  recre.classList.add('d-none');
  const maps = document.getElementById('mapsSidebar');
  maps.classList.add('d-none');
}

export function deactSpecies() {
  const species = document.getElementById('speciesSidebar');
  species.classList.add('d-none');
}

export function activeMaps() {
  const maps = document.getElementById('mapsSidebar');
  maps.classList.remove('d-none');
  maps.classList.add('d-block');
  const recre = document.getElementById('slidersSidebar');
  recre.classList.add('d-none');
  const species = document.getElementById('speciesSidebar');
  species.classList.add('d-none');
}

export function deactMaps() {
  const maps = document.getElementById('mapsSidebar');
  maps.classList.add('d-none');
}

export function fixTooltip(elId) {
  const sidebarButton = document.getElementById(elId);

  const domParser = new DOMParser();
  const spanHTML = domParser.parseFromString(sidebarButton.title, 'text/html');

  const tooltipOk = spanHTML.querySelector('.i18n').innerText;

  sidebarButton.title = tooltipOk;
}
