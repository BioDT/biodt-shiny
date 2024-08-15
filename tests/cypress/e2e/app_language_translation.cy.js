describe("The app's language translation", () => {
  /**
   * Cypress goes to the application's root URL. No need to specify full address 
   * because base URL is already configured in the Cypress config (tests/cypress.config.js).
   */
  beforeEach(() => {
    cy.visit('/')
  })

  it("defaults to EN in the beginning of the application", () => 
    {
      cy.get("#app-selected_language + div")
        .wait(20000)
        .click()
        //.get("select")
        //.select("en")
        //.should("have.value", "en")
    }
  )



  /*it("by clicking on 'Pollinators (Honeybee)' the screen changes to pDT 'Honeybee Beekeeper Case', and then gets back to homepage by clicking on BioDT logo", () => 
    {
      cy.get("#app-info-honeybee_selector")
        .should('be.visible')
        .click({force: true})
        .get("#app-biodt_logo")
        .click({force: true})
        .get(".title h2 span")
        .first()
        .should("have.text", "Species response to environmental change")
    }
  )*/
})
