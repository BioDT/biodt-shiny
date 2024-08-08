describe('app_homepage', () => {
  beforeEach(() => {
    cy.visit('/')
  })
  
  it("Loads app on landing page, showing top (h1) heading's first part 'Prototype'", () =>
    {
      cy.wait(1000)
      cy.get("#app-info-heading-first-part span").should("be.visible").should("have.text", "Prototype")
    }
  )

  
  it("Loads app on landing page, showing a rest of top (h1) heading", "Digital Twin", () =>
    {
      cy.wait(4000)
      cy.get("#app-info-heading-second-part span").should("be.visible").should("have.text", "Digital Twins")
    }
  )
})
