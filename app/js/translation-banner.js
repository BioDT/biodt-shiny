// Translation warning banner functionality

function showTranslationBanner() {
  // Check if user has dismissed the banner in this session
  if (sessionStorage.getItem('translationBannerDismissed') === 'true') {
    return;
  }
  
  const banner = document.getElementById('translation-warning-banner');
  if (banner) {
    banner.style.display = 'block';
  }
}

function hideTranslationBanner() {
  const banner = document.getElementById('translation-warning-banner');
  if (banner) {
    banner.style.display = 'none';
  }
}

function dismissTranslationBanner() {
  // Hide the banner
  hideTranslationBanner();
  
  // Remember dismissal for this session
  sessionStorage.setItem('translationBannerDismissed', 'true');
}

// Check on page load if we should show the banner
document.addEventListener('DOMContentLoaded', function() {
  // The banner will be shown by Shiny when a non-English language is selected
  // This just ensures it respects the session storage state
  const currentLang = document.documentElement.lang;
  if (currentLang && currentLang !== 'en' && 
      sessionStorage.getItem('translationBannerDismissed') !== 'true') {
    showTranslationBanner();
  }
});
