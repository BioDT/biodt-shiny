describe("The application", () => {
  /**
   * Cypress goes to the application's root URL. No need to specify full address 
   * because base URL is already configured in the Cypress config (tests/cypress.config.js).
   */
  beforeEach(() => {
    cy.visit('/')
  })

  it("starts", () => {})

  it("has correct (document) title", () =>
    {
      cy.title()
        .should('eq', 'BioDT')
    }
  )
  
  it("the first of three paragraphs with info text appears", () =>
    {
      cy.get(".info-text p:first-child span")
        .should("be.visible")
        .should("have.text", "The Biodiversity Digital Twin prototype will provide advanced models for simulation and prediction capabilities, through practical use cases addressing critical issues related to global biodiversity dynamics.")
    }
  )

  it("the second of three paragraphs with info text appears", () =>
    {
      cy.get(".info-text p:nth-child(2) span")
        .should("be.visible")
        .should("have.text", "BioDT exploits the LUMI Supercomputer and employs FAIR data combined with digital infrastructure, predictive modelling and AI solutions, facilitating evidence-based solutions for biodiversity protection and restoration.")
    }
  )

  it("the third of three paragraphs with info text appears", () =>
    {
      cy.get(".info-text p:nth-child(3) span")
        .should("be.visible")
        .should("have.text", "The project responds to key EU and international policy initiatives, including the EU Biodiversity Strategy 2030, EU Green Deal, UN Sustainable Development Goals, Destination Earth.")
    }
  )

  it("displays 4 'cards' with different categories of pDTs", () => 
    {
      cy.get(".landing-pdt-wrap")
        .should("have.length", 4)
    }
  )

  it("by clicking on 'Pollinators (Honeybee)' the screen changes to pDT 'Honeybee Beekeeper Case', and then gets back to homepage by clicking on BioDT logo", () => 
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
  )


})
