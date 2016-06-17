/*
 *  File: AtxSacMakeTables.cpp
 *
 *      Exports R functions to make normalized animal and impoundment data sets
 *      from input (intake, outcome, and impoundment) data sets.
 *      
 */

#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;
using std::vector;
using std::sort;
using std::string;
using std::map;
using std::pair;
using std::make_pair;
using std::shared_ptr;
using std::unique_ptr;
using std::make_shared;
using std::iterator;
using std::ostream;
using std::ostringstream;
using std::endl;




// RCpp as<string> converts any missing R character vectors to the string "NA".
// Therefore "NA" is a special string value.

static const string NaString = "NA";

// Common data frame column names.

namespace Col
{
    static const string RecSource = "rec_source";
    static const string AnimalId = "animal_id";
    static const string Gender = "gender";
    static const string Name = "name";
    static const string Kind = "kind";
    static const string Color1 = "color_1";
    static const string Color2 = "color_2";
    static const string Breed1 = "breed_1";
    static const string Breed2 = "breed_2";
    static const string Kennel = "kennel";
    static const string IntakeDate = "intake_date";
    static const string IntakeType = "intake_type";
    static const string IntakeSubType = "intake_subtype";
    static const string IntakeCondition = "intake_condition";
    static const string IntakeLocation = "intake_location";
    static const string IntakeAgeCount = "intake_age_count";
    static const string IntakeAgeUnits = "intake_age_units";
    static const string IntakeAge = "intake_age";
    static const string SpayNeuter = "spay_neuter";
    static const string IntakeSpayNeuter = "intake_spay_neuter";
    static const string OutcomeDate = "outcome_date";
    static const string OutcomeType = "outcome_type";
    static const string OutcomeSubType = "outcome_subtype";
    static const string OutcomeCondition = "outcome_condition";
    static const string OutcomeSpayNeuter = "outcome_spay_neuter";
}




/*
 *  Function: DateTimeToString
 *
 *      Converts an RCpp Datetime object to a printable string in the
 *      format mm/dd/yyyy hh:mm
 *      
 */
static string DateTimeToString (const Datetime& dateTime)
{
    using std::fixed;
    using std::setfill;
    using std::setw;
    
    ostringstream buffer;
    
    buffer << setfill('0') << setw(2)
           << dateTime.getMonth() << "/"
           << dateTime.getDay() << "/"
           << setw(4)
           << dateTime.getYear() << " "
           << setw(2)
           << dateTime.getHours() << ":"
           << dateTime.getMinutes();
    
    return buffer.str();
}




/*
 *  Class: WrapAsFactor
 *
 *      Converts a C++ vector of strings to a wrapped R vector of factors.
 *      
 */
static IntegerVector WrapAsFactor (const vector<string>& stringVector)
{
    // Convert the raw string vector to a wrapped R vector of character
    // vectors.

    CharacterVector charVector = wrap(stringVector);
    
    // Build an R character vector that contains unique strings; that
    // is, does not contain redundant duplicates of any strings.

    CharacterVector levelsVector = sort_unique(charVector);
    
    // Number of unique strings is the number of factor levels.
    // Now have to find the special literal string "NA" and remove
    // it, because we don't want that special string to become
    // a factor level.

    int numLevels = levelsVector.size();
    
    for (int i = 0; i < numLevels; ++i)
    {
        if (string((char*) levelsVector[i]) == "NA")
        {
            levelsVector.erase(i);
            break;
        }
    }
    
    // Create the integer vector of factor indexes by assigning sequential
    // numbers 1..numLevels to the unique strings, with "NA" removed.

    IntegerVector factorVector = match(charVector, levelsVector);
    
    // Make the vector an R vector of factors by assigning the right
    // class name and attaching the vector of level names.

    factorVector.attr("levels") = levelsVector;
    factorVector.attr("class") = "factor";
    
    return factorVector;
}




enum DayRelation
{
    EarlierDay,
    SameDay,
    LaterDay
};




/*
 *  Class: CompareByDay
 *
 *      Determines whether the first date-time is on the same day,
 *      an earlier day, or a later day than the second date-time.
 *      
 */
static DayRelation CompareByDay (const Datetime& dateTime1, const Datetime& dateTime2)
{
    int year1 = dateTime1.getYear();
    int year2 = dateTime2.getYear();
    
    if (year1 < year2)
        return EarlierDay;
    if (year1 > year2)
        return LaterDay;
    
    // Same year, check the month.

    int month1 = dateTime1.getMonth();
    int month2 = dateTime2.getMonth();
    
    if (month1 < month2)
        return EarlierDay;
    if (month1 > month2)
        return LaterDay;

    // Same month, check the day.

    int day1 = dateTime1.getDay();
    int day2 = dateTime2.getDay();
    
    if (day1 < day2)
        return EarlierDay;
    if (day1 > day2)
        return LaterDay;
    
    return SameDay;
}







/*** Intake ******************************************************************/

class Intake;
typedef shared_ptr<Intake> IntakeRef;




/*
 *  Class: Intake
 *
 *      Animal intake event.
 *      
 */
class Intake
{
public:
    Intake ();
    ~Intake () {}
    
    // Properties
    
    Datetime GetIntakeDate () const
    { return mIntakeDate; }

    void SetIntakeDate (const Datetime& intakeDate)
    { mIntakeDate = intakeDate; }

    string GetIntakeType () const
    { return mIntakeType; }
    
    void SetIntakeType (const string& intakeType)
    { mIntakeType = intakeType; }
    
    string GetIntakeSubType () const
    { return mIntakeSubType; }

    void SetIntakeSubType (const string& intakeSubType)
    { mIntakeSubType = intakeSubType; }
    
    string GetIntakeCondition () const
    { return mIntakeCondition; }

    void SetIntakeCondition (const string& intakeCondition)
    { mIntakeCondition = intakeCondition; }

    string GetIntakeLocation () const
    { return mIntakeLocation; }

    void SetIntakeLocation (const string& intakeLocation)
    { mIntakeLocation = intakeLocation; }

    int GetIntakeAgeCount () const
    { return mIntakeAgeCount; }

    void SetIntakeAgeCount (int intakeAgeCount)
    { mIntakeAgeCount = intakeAgeCount; }

    string GetIntakeAgeUnits () const
    { return mIntakeAgeUnits; }
    
    void SetIntakeAgeUnits (const string& intakeAgeUnits)
    { mIntakeAgeUnits = intakeAgeUnits; }

    int GetIntakeAge () const
    { return mIntakeAge; }
    
    void SetIntakeAge (int intakeAge)
    { mIntakeAge = intakeAge; }

    string GetIntakeSpayNeuter () const
    { return mIntakeSpayNeuter; }
    
    void SetIntakeSpayNeuter (const string& intakeSpayNeuter)
    { mIntakeSpayNeuter = intakeSpayNeuter; }

    string GetKennel () const
    { return mKennel; }

    void SetKennel (const string& kennel)
    { mKennel = kennel; }
    
    // Convert to printable string.

    string ToString () const;
    
private:
    
    Datetime mIntakeDate;       // Intake event timestamp
    string mIntakeType;         // Type of intake (e.g., Stray, Owner Surrender)
    string mIntakeSubType;      // Sub-type of intake type (e.g., Stray/Field, Owner Surrender/OTC)
    string mIntakeCondition;    // Condition at time of intake (e.g., Normal, Injured)
    string mIntakeLocation;     // Place where animal was captured or surrendered
    int mIntakeAgeCount;        // Integer age
    string mIntakeAgeUnits;     // Units of the integer age (e.g., dy, mo, yr)
    int mIntakeAge;             // Age represented as a count of seconds (denormalized age count)
    string mIntakeSpayNeuter;   // Sterilization status (e.g., Intact, Altered)
    string mKennel;             // Kennel assignment
};




/*
 *  Function: IntakeTimeLessThan
 *
 *      Compares the intake timestamps of two intake objects and returns
 *      whether the first is less than the second.
 *      
 *      Used when sorting a container of intake objects by intake date-time.
 *      
 */
static bool IntakeTimeLessThan (const IntakeRef& intakeA, const IntakeRef& intakeB)
{
    return (intakeA->GetIntakeDate() < intakeB->GetIntakeDate());
}




/*
 *  Method: Null constructor
 *
 *      Initializes this object to NA.
 *      
 */
Intake::Intake ()
       :
        mIntakeDate(NA_REAL),
        mIntakeType(NaString),
        mIntakeSubType(NaString),
        mIntakeCondition(NaString),
        mIntakeLocation(NaString),
        mIntakeAgeCount(NA_INTEGER),
        mIntakeAgeUnits(NaString),
        mIntakeAge(NA_INTEGER),
        mIntakeSpayNeuter(NaString),
        mKennel(NaString)
{
}




/*
 *  Method: ToString
 *
 *      Returns the printable string representation of this object.
 *      
 */
string Intake::ToString () const
{
    ostringstream buffer;
    
    buffer << "Intake " << DateTimeToString(mIntakeDate)
           << " type(" << mIntakeType
           << ") subtype(" << mIntakeSubType
           << ") condition(" << mIntakeCondition
           << ") spayNeuter(" << mIntakeSpayNeuter
           << ") ageCount(" << mIntakeAgeCount
           << ") ageUnits(" << mIntakeAgeUnits
           << ") age(" << mIntakeAge
           << ") location(" << mIntakeLocation
           << ") kennel(" << mKennel
           << ")";
    
    return buffer.str();
}




/*** Outcome *****************************************************************/

class Outcome;
typedef shared_ptr<Outcome> OutcomeRef;




/*
 *  Class: Outcome
 *
 *      Animal outcome event.
 *      
 */
class Outcome
{
public:
    Outcome ();
    ~Outcome () {}

    // Properties

    Datetime GetOutcomeDate () const
    { return mOutcomeDate; }
    
    void SetOutcomeDate (const Datetime& outcomeDate)
    { mOutcomeDate = outcomeDate; }

    string GetOutcomeType () const
    { return mOutcomeType; }
    
    void SetOutcomeType (const string& outcomeType)
    { mOutcomeType = outcomeType; }

    string GetOutcomeSubType () const
    { return mOutcomeSubType; }
    
    void SetOutcomeSubType (const string& outcomeSubType)
    { mOutcomeSubType = outcomeSubType; }

    string GetOutcomeCondition () const
    { return mOutcomeCondition; }
    
    void SetOutcomeCondition (const string& outcomeCondition)
    { mOutcomeCondition = outcomeCondition; }

    string GetOutcomeSpayNeuter () const
    { return mOutcomeSpayNeuter; }

    void SetOutcomeSpayNeuter (const string& outcomeSpayNeuter)
    { mOutcomeSpayNeuter = outcomeSpayNeuter; }

    // Convert to printable string.

    string ToString () const;
    
private:
    
    Datetime mOutcomeDate;      // Intake event timestamp
    string mOutcomeType;        // Type of outcome (e.g., Adoption, Transfer, Return to Owner)
    string mOutcomeSubType;     // Sub-type of outcome type (e.g., Adoption/Foster, Transfer/Partner)
    string mOutcomeCondition;   // Condition at time of discharge (e.g., Normal, Sick)
    string mOutcomeSpayNeuter;  // Sterilization status when discharged (e.g., Intact, Altered)
};




/*
 *  Function: OutcomeTimeLessThan
 *
 *      Compares the outcome timestamps of two outcome objects and returns
 *      whether the first is less than the second.
 *      
 *      Used when sorting a container of outcome objects by outcome date-time.
 *      
 */
static bool OutcomeTimeCompare (const OutcomeRef& outcomeA, const OutcomeRef& outcomeB)
{
    return (outcomeA->GetOutcomeDate() < outcomeB->GetOutcomeDate());
}




/*
 *  Method: Null constructor
 *
 *      Initializes this object to NA.
 *      
 */
Outcome::Outcome ()
        :
         mOutcomeDate(NA_REAL),
         mOutcomeType(NaString),
         mOutcomeSubType(NaString),
         mOutcomeCondition(NaString),
         mOutcomeSpayNeuter(NaString)
{
}




/*
 *  Method: ToString
 *
 *      Returns the printable string representation of this object.
 *      
 */
string Outcome::ToString () const
{
    ostringstream buffer;

    buffer << "Outcome " << DateTimeToString(mOutcomeDate)
           << " type(" << mOutcomeType
           << ") subtype(" << mOutcomeSubType
           << ") spayNeuter(" << mOutcomeSpayNeuter
           << ")";

    return buffer.str();
}





/*** Animal ******************************************************************/

class Animal;
typedef shared_ptr<Animal> AnimalRef;




/*
 *  Class: Animal
 *
 *      Animal description.
 *      
 */
class Animal
{
public:
    
    Animal (const string& animalId, const Datetime& dateTime);
    ~Animal () {}

    // Add intake or outcome events for this animal.

    void AddIntake (const IntakeRef& intake);
    void AddOutcome (const OutcomeRef& outcome);
    
    // Update fields from another animal.
    
    void UpdateIfNewer (const AnimalRef& animal);

    // Properties

    int GetNumIntakes () const
    { return mIntakeList.size(); }
    
    IntakeRef GetIntakeAt (int i) const
    { return mIntakeList.at(i); }

    int GetNumOutcomes () const
    { return mOutcomeList.size(); }
    
    OutcomeRef GetOutcomeAt (int i) const
    { return mOutcomeList.at(i); }
    
    // Sort the intake or outcome events for this animal.

    void SortIntakes ();
    void SortOutcomes ();
    
    // Properties
    
    string GetAnimalId () const
    { return mAnimalId; }

    void SetAnimalId (const string& animalId)
    { mAnimalId = animalId; }

    string GetKind () const
    { return mKind; }

    void SetKind (const string& kind)
    { mKind = kind; }
    
    string GetName () const
    { return mName; }
    
    void SetName (const string& name)
    { mName = name; }

    string GetGender () const
    { return mGender; }
    
    void SetGender (const string& gender)
    { mGender = gender; }

    string GetColor1 () const
    { return mColor1; }
    
    void SetColor1 (const string& color1)
    { mColor1 = color1; }

    string GetColor2 () const
    { return mColor2; }
    
    void SetColor2 (const string& color2)
    { mColor2 = color2; }

    string GetBreed1 () const
    { return mBreed1; }
    
    void SetBreed1 (const string& breed1)
    { mBreed1 = breed1; }

    string GetBreed2 () const
    { return mBreed2; }
    
    void SetBreed2 (const string& breed2)
    { mBreed2 = breed2; }

    Datetime GetDateTime () const
    { return mDateTime; }

    // Convert to printable string.

    string ToString () const;

    // Print this animal and all intake and outcome events.

    void DeepPrint (ostream& output) const;

private:

    void RemoveIntakeAt (int index)
    { mIntakeList.erase(mIntakeList.begin() + index); }

private:

    string mAnimalId;   // Impound identifier for this animal
    string mKind;       // Kind (e.g., Dog, Cat)
    string mGender;     // Gender (e.g., Male, Female)
    string mName;       // Name
    string mColor1;     // Primary color
    string mColor2;     // Secondary color
    string mBreed1;     // Primary breed designation
    string mBreed2;     // Secondary breed designation
    Datetime mDateTime; // Timestamp of this animal's information
    
    typedef vector<IntakeRef> IntakeList;
    typedef vector<OutcomeRef> OutcomeList;

    IntakeList mIntakeList;      // List of intake events
    OutcomeList mOutcomeList;    // List of outcome events
};




/*
 *  Method: Null constructor
 *
 *      Initializes this object to NA.
 *      
 */
Animal::Animal (const string& animalId, const Datetime& dateTime)
       :
        mAnimalId(animalId),
        mKind(NaString),
        mGender(NaString),
        mName(NaString),
        mColor1(NaString),
        mColor2(NaString),
        mBreed1(NaString),
        mBreed2(NaString),
        mDateTime(dateTime),
        mIntakeList(),
        mOutcomeList()
{
}




/*
 *  Method: UpdateIfNewer
 *
 *      Updates this animal's information from the specified animal when
 *      the information is newer.
 *
 *      Update will never delete accumulated information (i.e., convert a field
 *      to empty because the update-from animal's field is empty). 
 *      
 */
void Animal::UpdateIfNewer (const AnimalRef& animal)
{
    // Do not update if the source animal has older information.

    if (animal->GetDateTime() <= GetDateTime())
        return;
    
    // Never overwrite an existing field with a newer field that has possibly
    // been deleted (i.e., a newer field that has value NA).
    
    string name = animal->GetName();
    if (name != NaString)
        SetName(name);
    
    string gender = animal->GetGender();
    if (gender != NaString)
        SetGender(gender);
    
    string color1 = animal->GetColor1();
    if (color1 != NaString)
        SetColor1(color1);
    
    string color2 = animal->GetColor2();
    if (color2 != NaString)
        SetColor2(color2);
    
    string breed1 = animal->GetBreed1();
    if (breed1 != NaString)
        SetBreed1(breed1);
    
    string breed2 = animal->GetBreed2();
    if (breed2 != NaString)
        SetBreed2(breed2);
    
    // Advance the timestamp to that of the source animal.

    mDateTime = animal->GetDateTime();
}




/*
 *  Method: AddIntake
 *
 *      Adds the specified intake event for this animal.
 *      
 */
void Animal::AddIntake (const IntakeRef& intake)
{
    mIntakeList.push_back(intake);
}




/*
 *  Method: AddOutcome
 *
 *      Adds the specified outcome event for this animal.
 *      
 */
void Animal::AddOutcome (const OutcomeRef& outcome)
{
    mOutcomeList.push_back(outcome);
}




/*
 *  Method: SortIntakes
 *
 *      Sorts the intake events for this animal.
 *      
 */
void Animal::SortIntakes ()
{
    sort(mIntakeList.begin(), mIntakeList.end(), IntakeTimeLessThan);
}




/*
 *  Method: SortOutcomes
 *
 *      Sorts the outcome events for this animal.
 *      
 */
void Animal::SortOutcomes ()
{
    sort(mOutcomeList.begin(), mOutcomeList.end(), OutcomeTimeCompare);
}




/*
 *  Method: ToString
 *
 *      Returns the printable string representation of this object.
 *      
 */
string Animal::ToString () const
{
    ostringstream buffer;

    buffer << "Animal " << mAnimalId
           << " kind(" << mKind
           << ") gender(" << mGender
           << ") name(" << mName
           << ") color(" << mColor1 << "," << mColor2
           << ") breed(" << mBreed1 << "," << mBreed2
           << ")";

    return buffer.str();
}




/*
 *  Method: DeepPrint
 *
 *      Prints a complete representation of this animal to the specified
 *      output stream.
 *      
 */
void Animal::DeepPrint (ostream& output) const
{
    // Output this animal's  description.

    output << ToString() << endl;

    // Output intake and outcome events interleaved as they appear
    // on their respective lists.

    int numIntakesRemaining = GetNumIntakes();
    int numOutcomesRemaining = GetNumOutcomes();
    int nextIntake = 0;
    int nextOutcome = 0;

    while (numIntakesRemaining > 0 || numOutcomesRemaining > 0)
    {
        if (numIntakesRemaining > 0)
        {
            IntakeRef intake = GetIntakeAt(nextIntake);
            ++nextIntake;
            --numIntakesRemaining;
            
            output << intake->ToString() << endl;
        }

        if (numOutcomesRemaining > 0)
        {
            OutcomeRef outcome = GetOutcomeAt(nextOutcome);
            ++nextOutcome;
            --numOutcomesRemaining;
            
            output << outcome->ToString() << endl;
        }
    }
}




/*** AnimalMap ***************************************************************/

/*
 *  Class: AnimalMap
 *
 *      Dictionary of animal objects keyed on animal ID.
 *      
 */
class AnimalMap : public map<string, AnimalRef>
{
public:

    // Find an animal.

    AnimalRef Lookup (const string& animalId) const;
    
    // Add or replace an animal.

    void Add (const AnimalRef& animal);
};




/*
 *  Method: Lookup
 *
 *      Look up an animal object by its animal ID.
 *      
 *      Returns a pointer to the animal object, or nullptr when the
 *      animal is not found.
 *      
 */
AnimalRef AnimalMap::Lookup (const string& animalId) const
{
    AnimalRef animal = nullptr;
    const_iterator iter = find(animalId);
    
    if (iter != end())
    {
        animal = iter->second;
    }

    return animal;
}




/*
 *  Method: Add
 *
 *      Add an animal object to this dictionary or replace an existing
 *      animal object.
 *      
 */
void AnimalMap::Add (const AnimalRef& animal)
{
    if (animal == nullptr)
        return;

    (*this)[animal->GetAnimalId()] = animal;
}




/*** AnimalTable *************************************************************/

/*
 *  Class: AnimalTable
 *
 *      Accumulator to build animal data frame.
 *      
 */
class AnimalTable
{
public:
    AnimalTable () {}
    ~AnimalTable () {}

    void Append (const AnimalRef& animal);
    void Clear ();

    DataFrame GetDataFrame () const;
        
private:
    // Vectors acculumate the columns of this animal table.

    vector<string> mAnimalIdCol;
    vector<string> mNameCol;
    vector<string> mKindCol;
    vector<string> mGenderCol;
    vector<string> mColor1Col;
    vector<string> mColor2Col;
    vector<string> mBreed1Col;
    vector<string> mBreed2Col;
};




/*
 *  Method: Append
 *
 *      Add an animal as a new row appended to this table.
 *      
 */
void AnimalTable::Append (const AnimalRef& animal)
{
    // Table is stored as columns; so appending a row appends to each column.

    mAnimalIdCol.push_back(animal->GetAnimalId());
    mNameCol.push_back(animal->GetName());
    mKindCol.push_back(animal->GetKind());
    mGenderCol.push_back(animal->GetGender());
    mColor1Col.push_back(animal->GetColor1());
    mColor2Col.push_back(animal->GetColor2());
    mBreed1Col.push_back(animal->GetBreed1());
    mBreed2Col.push_back(animal->GetBreed2());
}




/*
 *  Method: Clear
 *
 *      Remove all rows from this animal table.
 *      
 */
void AnimalTable::Clear ()
{
    mAnimalIdCol.clear();
    mNameCol.clear();
    mKindCol.clear();
    mGenderCol.clear();
    mColor1Col.clear();
    mColor2Col.clear();
    mBreed1Col.clear();
    mBreed2Col.clear();
}




/*
 *  Method: GetDataFrame
 *
 *      Creates an R data frame object corresponding to the rows of
 *      animals in this table.
 *      
 */
DataFrame AnimalTable::GetDataFrame () const
{
    using namespace Col;

    // Each named column vector is converted to an R vector object,
    // in this case factor (integer) vectors.

    return DataFrame::create(Named(AnimalId) = WrapAsFactor(mAnimalIdCol),
                             Named(Kind) = WrapAsFactor(mKindCol),
                             Named(Name) = WrapAsFactor(mNameCol),
                             Named(Gender) = WrapAsFactor(mGenderCol),
                             Named(Color1) = WrapAsFactor(mColor1Col),
                             Named(Color2) = WrapAsFactor(mColor2Col),
                             Named(Breed1) = WrapAsFactor(mBreed1Col),
                             Named(Breed2) = WrapAsFactor(mBreed2Col));
}




/*** ImpoundTable ************************************************************/

/*
 *  Class: ImpoundTable
 *
 *      Accumulator to build impound data frame.
 *      
 */
class ImpoundTable
{
public:
    ImpoundTable () {}
    ~ImpoundTable () {}
    
    void Append (const AnimalRef& animal, const IntakeRef& intake, const OutcomeRef& outcome);
    void Clear ();
        
    DataFrame GetDataFrame () const;
    
private:
    vector<string> mAnimalIdCol;
    vector<Datetime> mIntakeDateCol;
    vector<string> mIntakeTypeCol;
    vector<string> mIntakeSubTypeCol;
    vector<string> mIntakeConditionCol;
    vector<string> mIntakeLocationCol;
    vector<int> mIntakeAgeCountCol;
    vector<string> mIntakeAgeUnitsCol;
    vector<int> mIntakeAgeCol;
    vector<string> mIntakeSpayNeuterCol;
    vector<Datetime> mOutcomeDateCol;
    vector<string> mOutcomeTypeCol;
    vector<string> mOutcomeSubTypeCol;
    vector<string> mOutcomeConditionCol;
    vector<string> mOutcomeSpayNeuterCol;
    vector<string> mKennelCol;
};




/*
 *  Class: Append
 *
 *      Add an impound as a new row appended to this table.
 *      
 */
void ImpoundTable::Append (const AnimalRef& animal, const IntakeRef& intake, const OutcomeRef& outcome)
{
    mAnimalIdCol.push_back(animal->GetAnimalId());
    
    mIntakeDateCol.push_back(intake->GetIntakeDate());
    mIntakeTypeCol.push_back(intake->GetIntakeType());
    mIntakeSubTypeCol.push_back(intake->GetIntakeSubType());
    mIntakeConditionCol.push_back(intake->GetIntakeCondition());
    mIntakeLocationCol.push_back(intake->GetIntakeLocation());
    mIntakeAgeCountCol.push_back(intake->GetIntakeAgeCount());
    mIntakeAgeUnitsCol.push_back(intake->GetIntakeAgeUnits());
    mIntakeAgeCol.push_back(intake->GetIntakeAge());
    mIntakeSpayNeuterCol.push_back(intake->GetIntakeSpayNeuter());
    mKennelCol.push_back(intake->GetKennel());
    
    mOutcomeDateCol.push_back(outcome->GetOutcomeDate());
    mOutcomeTypeCol.push_back(outcome->GetOutcomeType());
    mOutcomeSubTypeCol.push_back(outcome->GetOutcomeSubType());
    mOutcomeConditionCol.push_back(outcome->GetOutcomeCondition());
    mOutcomeSpayNeuterCol.push_back(outcome->GetOutcomeSpayNeuter());
    
}




/*
 *  Method: Clear
 *
 *      Remove all rows from this impound table.
 *      
 */
void ImpoundTable::Clear ()
{
    mAnimalIdCol.clear();
    mIntakeDateCol.clear();
    mIntakeTypeCol.clear();
    mIntakeSubTypeCol.clear();
    mIntakeConditionCol.clear();
    mIntakeLocationCol.clear();
    mIntakeAgeCountCol.clear();
    mIntakeAgeUnitsCol.clear();
    mIntakeAgeCol.clear();
    mIntakeSpayNeuterCol.clear();
    mOutcomeDateCol.clear();
    mOutcomeTypeCol.clear();
    mOutcomeSubTypeCol.clear();
    mOutcomeConditionCol.clear();
    mOutcomeSpayNeuterCol.clear();
    mKennelCol.clear();
}




/*
 *  Class: GetDataFrame
 *
 *      Creates an R data frame object corresponding to the rows of
 *      impounds in this table.
 *      
 */
DataFrame ImpoundTable::GetDataFrame () const
{
    using namespace Col;

    // Each named column vector is converted to an R vector object,
    // either afactor (integer) vector, a numeric (real) vector, or
    // a date-time (POSIXct) vector.
    
    return DataFrame::create(Named(AnimalId) = WrapAsFactor(mAnimalIdCol),
                             Named(IntakeDate) = wrap(mIntakeDateCol),
                             Named(IntakeType) = WrapAsFactor(mIntakeTypeCol),
                             Named(IntakeSubType) = WrapAsFactor(mIntakeSubTypeCol),
                             Named(IntakeCondition) = WrapAsFactor(mIntakeConditionCol),
                             Named(IntakeLocation) = WrapAsFactor(mIntakeLocationCol),
                             Named(IntakeAgeCount) = wrap(mIntakeAgeCountCol),
                             Named(IntakeAgeUnits) = WrapAsFactor(mIntakeAgeUnitsCol),
                             Named(IntakeAge) = wrap(mIntakeAgeCol),
                             Named(IntakeSpayNeuter) = WrapAsFactor(mIntakeSpayNeuterCol),
                             Named(Kennel) = WrapAsFactor(mKennelCol),
                             Named(OutcomeDate) = wrap(mOutcomeDateCol),
                             Named(OutcomeType) = WrapAsFactor(mOutcomeTypeCol),
                             Named(OutcomeSubType) = WrapAsFactor(mOutcomeSubTypeCol),
                             Named(OutcomeCondition) = WrapAsFactor(mOutcomeConditionCol),
                             Named(OutcomeSpayNeuter) = WrapAsFactor(mOutcomeSpayNeuterCol));
}




/*** DataFrameBuilder ********************************************************/

/*
 *  Class: DataFrameBuilder
 *
 *      Builds separate animal and impound data frames from either combined or
 *      disjoint intake and outcome input data frames in various expected
 *      formats.
 *      
 */
class DataFrameBuilder
{
public:
    DataFrameBuilder () {}
    ~DataFrameBuilder () {}

    // Build animal table and impound table from different sorts
    // of input records.

    void BuildFromAtxIntakesAndOutcomes (const DataFrame& intake, const DataFrame& outcome);
    void BuildFromSacOpenImpounds (const DataFrame& impound);
    void BuildFromSacCpraImpounds (const DataFrame& impound);
        
    // Properties
    
    DataFrame GetAnimalDataFrame () const
    { return mAnimalTable.GetDataFrame(); }
    
    DataFrame GetImpoundDataFrame () const
    { return mImpoundTable.GetDataFrame(); }

private:
    void Clear ();
    void IngestAtxIntakes (const DataFrame& intakeTable);
    void IngestAtxOutcomes (const DataFrame& outcomeTable);
    void IngestSacOpenImpounds (const DataFrame& impoundTable);
    void IngestSacCpraImpounds (const DataFrame& impoundTable);
    
    AnimalRef AddAnimal (const AnimalRef& animal);

    void MergeAnimal (const AnimalRef& animal);
    void EmitSolitaryIntake (const AnimalRef& animal, const IntakeRef& intake);
    void EmitIntakeOutcome (const AnimalRef& animal, const IntakeRef& intake, const OutcomeRef& outcome);
    void EmitSolitaryOutcome (const AnimalRef& animal, const OutcomeRef& outcome);
    void BuildAnimalTable ();
    void BuildImpoundTable ();

    void Warning (const AnimalRef& animal, const string& message) const;
    
private:
    AnimalMap mAnimalMap;           // Dictionary of individual animals
    AnimalTable mAnimalTable;       // Output data table of animals
    ImpoundTable mImpoundTable;     // Output data table of animal impounds
};




/*
 *  Class: Clear
 *
 *      Erases all internal data structures.
 *      Leaves this builder ready to build new tables.
 *      
 */
void DataFrameBuilder::Clear ()
{
    mAnimalTable.Clear();
    mImpoundTable.Clear();
    mAnimalMap.clear();
}




/*
 *  Class: BuildFromAtxIntakesAndOutcomes
 *
 *      Builds tables from input intake and outcome events.
 *      
 */
void DataFrameBuilder::BuildFromAtxIntakesAndOutcomes (const DataFrame& intake, const DataFrame& outcome)
{
    // Erase previous tables built.

    Clear();

    // Build the table of impounds.

    IngestAtxIntakes(intake);
    IngestAtxOutcomes(outcome);
    BuildImpoundTable();
    
    // Build the table of animals.

    BuildAnimalTable();
}




/*
 *  Class: BuildFromSacOpenImpounds
 *
 *      Builds tables from input open-data impound events, which combine intake
 *      and outcome events.
 *      
 */
void DataFrameBuilder::BuildFromSacOpenImpounds (const DataFrame& impound)
{
    // Erase previous tables built.

    Clear();

    // Build the table of impounds.
    
    IngestSacOpenImpounds(impound);
    BuildImpoundTable();

    // Build the table of animals.
    
    BuildAnimalTable();
}




/*
 *  Class: BuildFromSacCpraImpounds
 *
 *      Builds tables from input CPRA impound events, which combine intake
 *      and outcome events.
 *      
 */
void DataFrameBuilder::BuildFromSacCpraImpounds (const DataFrame& impound)
{
    // Erase previous tables built.
    
    Clear();
    
    // Build the table of impounds.
    
    IngestSacCpraImpounds(impound);
    BuildImpoundTable();
    
    // Build the table of animals.
    
    BuildAnimalTable();
}




/*
 *  Class: BuildAnimalTable
 *
 *      Builds the animal table from the animal map created from
 *      the input data.
 *      
 */
void DataFrameBuilder::BuildAnimalTable ()
{
    // Build the animal table from the animals in the animal map.

    AnimalMap::const_iterator iter;

    for (iter = mAnimalMap.begin(); iter != mAnimalMap.end(); ++iter)
    {
        AnimalRef animal = iter->second;
        
        mAnimalTable.Append(animal);
    }
}




/*
 *  Class: AddAnimal
 *
 *      Add a new animal or update an existing animal.
 *      
 *      Returns a pointer to the animal object that was updated, which
 *      may not be the animal object that was passed as an input argument.
 *      
 */
AnimalRef DataFrameBuilder::AddAnimal (const AnimalRef& animal)
{
    // Look up the animal to see if it is already in the dictionary.

    AnimalRef existingAnimal = mAnimalMap.Lookup(animal->GetAnimalId());
    
    // Update an animal that has been seen before. Otherwise create
    // a new animal.

    if (existingAnimal != nullptr)
    {
        // Only update when the incoming information is (from a record) more recent than
        // the existing animal's informaton.

        existingAnimal->UpdateIfNewer(animal);

        return existingAnimal;
    } 
    else
    {
        // Add the new animal to the dictionary.

        mAnimalMap.Add(animal);
        
        return animal;
    }
}




/*
 *  Method: Warning
 *
 *      Prints a warning message to the console.
 *      
 */
void DataFrameBuilder::Warning (const AnimalRef& animal, const string& message) const
{
    Rcout << "WARNING " << animal->GetAnimalId() << " - " << message << endl;
}




/*
 *  Method: IngestAtxIntakes
 *
 *      Adds the given Austin intake records to the internal representation of
 *      animals and events.  
 *      
 */
void DataFrameBuilder::IngestAtxIntakes (const DataFrame& intakeTable)
{
    int numIntakes = intakeTable.nrows();
    if (numIntakes == 0)
        return;

    using namespace Col;

    // Get the R data frame columns wrapped as C++ objects.

    CharacterVector animalIdCol =  intakeTable[AnimalId];
    CharacterVector kindCol = intakeTable[Kind];
    CharacterVector genderCol = intakeTable[Gender];
    CharacterVector nameCol = intakeTable[Name];
    CharacterVector color1Col = intakeTable[Color1];
    CharacterVector color2Col = intakeTable[Color2];
    CharacterVector breed1Col = intakeTable[Breed1];
    CharacterVector breed2Col = intakeTable[Breed2];
    
    DatetimeVector intakeDateCol = intakeTable[IntakeDate];
    CharacterVector intakeTypeCol = intakeTable[IntakeType];
    CharacterVector intakeConditionCol = intakeTable[IntakeCondition];
    CharacterVector intakeLocationCol = intakeTable[IntakeLocation];
    IntegerVector intakeAgeCountCol = intakeTable[IntakeAgeCount];
    CharacterVector intakeAgeUnitsCol = intakeTable[IntakeAgeUnits];
    IntegerVector intakeAgeCol = intakeTable[IntakeAge];
    CharacterVector intakeSpayNeuterCol = intakeTable[IntakeSpayNeuter];

    // Add rows of animals and intakes to the internal accumulator tables.

    for (int i = 0; i < numIntakes; ++i)
    {
        string animalId = as<string>(animalIdCol[i]);
        Datetime intakeDate = intakeDateCol[i];

        // Create an animal object from the animal information in the
        // intake record.

        AnimalRef animal = make_shared<Animal>(animalId, intakeDate);
        animal->SetKind(as<string>(kindCol[i]));
        animal->SetGender(as<string>(genderCol[i]));
        animal->SetName(as<string>(nameCol[i]));
        animal->SetColor1(as<string>(color1Col[i]));
        animal->SetColor2(as<string>(color2Col[i]));
        animal->SetBreed1(as<string>(breed1Col[i]));
        animal->SetBreed2(as<string>(breed2Col[i]));

        // Add or update the animal in the internal map.
        // The returned pointer is to the animal object stored in
        // the internal map.
        
        animal = AddAnimal(animal);

        // Create an intake object from the intake information in the
        // intake record. Add the intake object to the internal map.

        IntakeRef intake = make_shared<Intake>();
        intake->SetIntakeDate(intakeDateCol[i]);
        intake->SetIntakeType(as<string>(intakeTypeCol[i]));
        intake->SetIntakeCondition(as<string>(intakeConditionCol[i]));
        intake->SetIntakeLocation(as<string>(intakeLocationCol[i]));
        intake->SetIntakeAgeCount(intakeAgeCountCol[i]);
        intake->SetIntakeAgeUnits(as<string>(intakeAgeUnitsCol[i]));
        intake->SetIntakeAge(intakeAgeCol[i]);
        intake->SetIntakeSpayNeuter(as<string>(intakeSpayNeuterCol[i]));
        
        animal->AddIntake(intake);
    }
}




/*
 *  Method: IngestAtxOutcomes
 *
 *      Adds the given Austin outcome records to the internal representation of
 *      animals and events.  
 *      
 */
void DataFrameBuilder::IngestAtxOutcomes (const DataFrame& outcomeTable)
{
    int numOutcomes = outcomeTable.nrows();
    if (numOutcomes == 0)
        return;
    
    using namespace Col;

    // Get the R data frame columns wrapped as C++ objects.

    CharacterVector animalIdCol = outcomeTable[AnimalId];
    CharacterVector kindCol = outcomeTable[Kind];
    CharacterVector genderCol = outcomeTable[Gender];
    CharacterVector nameCol = outcomeTable[Name];
    CharacterVector color1Col = outcomeTable[Color1];
    CharacterVector color2Col = outcomeTable[Color2];
    CharacterVector breed1Col = outcomeTable[Breed1];
    CharacterVector breed2Col = outcomeTable[Breed2];
    DatetimeVector outcomeDateCol = outcomeTable[OutcomeDate];
    CharacterVector outcomeTypeCol = outcomeTable[OutcomeType];
    CharacterVector outcomeSubTypeCol = outcomeTable[OutcomeSubType];
    CharacterVector outcomeSpayNeuterCol = outcomeTable[OutcomeSpayNeuter];
    
    // Add rows of animals and outcomes to the internal accumulator tables.

    for (int i = 0; i < numOutcomes; ++i)
    {
        string animalId = as<string>(animalIdCol[i]);
        Datetime outcomeDate = outcomeDateCol[i];
        
        // Create an animal object from the animal information in the
        // outcome record.

        AnimalRef animal = make_shared<Animal>(animalId, outcomeDate);
        animal->SetKind(as<string>(kindCol[i]));
        animal->SetGender(as<string>(genderCol[i]));
        animal->SetName(as<string>(nameCol[i]));
        animal->SetColor1(as<string>(color1Col[i]));
        animal->SetColor2(as<string>(color2Col[i]));
        animal->SetBreed1(as<string>(breed1Col[i]));
        animal->SetBreed2(as<string>(breed2Col[i]));
        
        // Add or update the animal in the internal map.
        // The returned pointer is to the animal object stored in
        // the internal map.

        animal = AddAnimal(animal);

        // Create an outcome object from the outcome information in the
        // outcome record. Add the outcome object to the internal map.

        OutcomeRef outcome = make_shared<Outcome>();
        outcome->SetOutcomeDate(outcomeDateCol[i]);
        outcome->SetOutcomeType(as<string>(outcomeTypeCol[i]));
        outcome->SetOutcomeSubType(as<string>(outcomeSubTypeCol[i]));
        outcome->SetOutcomeSpayNeuter(as<string>(outcomeSpayNeuterCol[i]));
        
        animal->AddOutcome(outcome);
    }
}




/*
 *  Method: IngestSacOpenImpounds
 *
 *      Adds the given open-data Sacramento impound (i.e., intake and outcome)
 *      records to the internal representation of animals and events.
 *      
 */
void DataFrameBuilder::IngestSacOpenImpounds (const DataFrame& impoundTable)
{
    int numImpounds = impoundTable.nrows();
    if (numImpounds == 0)
        return;
    
    using namespace Col;

    // Get the R data frame columns wrapped as C++ objects.
    
    CharacterVector animalIdCol = impoundTable[AnimalId];
    CharacterVector kindCol = impoundTable[Kind];
    CharacterVector nameCol = impoundTable[Name];
    
    DatetimeVector intakeDateCol = impoundTable[IntakeDate];
    CharacterVector intakeTypeCol = impoundTable[IntakeType];
    CharacterVector intakeLocationCol = impoundTable[IntakeLocation];
    DatetimeVector outcomeDateCol = impoundTable[OutcomeDate];
    CharacterVector outcomeTypeCol = impoundTable[OutcomeType];

    // Add rows of animals and intakes and outcomes to the internal accumulator tables.
    
    for (int i = 0; i < numImpounds; ++i)
    {
        string animalId = as<string>(animalIdCol[i]);
        Datetime intakeDate = intakeDateCol[i];
        
        // Create an animal object from the animal information in the
        // impound record.

        AnimalRef animal = make_shared<Animal>(animalId, intakeDate);
        animal->SetKind(as<string>(kindCol[i]));
        animal->SetName(as<string>(nameCol[i]));
        
        // Add or update the animal in the internal map.
        // The returned pointer is to the animal object stored in
        // the internal map.

        animal = AddAnimal(animal);

        // Create intake and outcome objects from the information in the
        // impound record. Add the intake-outcome pair to the internal map.

        IntakeRef intake = make_shared<Intake>();
        intake->SetIntakeDate(intakeDateCol[i]);
        intake->SetIntakeType(as<string>(intakeTypeCol[i]));
        intake->SetIntakeLocation(as<string>(intakeLocationCol[i]));

        animal->AddIntake(intake);

        OutcomeRef outcome = make_shared<Outcome>();
        outcome->SetOutcomeDate(outcomeDateCol[i]);
        outcome->SetOutcomeType(as<string>(outcomeTypeCol[i]));

        animal->AddOutcome(outcome);
    }
}




/*
 *  Method: IngestSacCpraImpounds
 *
 *      Adds the given CPRA (California Public Records Act) Sacramento
 *      impound (i.e., intake and outcome) records to the internal
 *      representation of animals and events.
 *      
 */
void DataFrameBuilder::IngestSacCpraImpounds (const DataFrame& impoundTable)
{
    int numImpounds = impoundTable.nrows();
    if (numImpounds == 0)
        return;
    
    using namespace Col;

    // Get the R data frame columns wrapped as C++ objects.
    
    CharacterVector animalIdCol = impoundTable[AnimalId];
    CharacterVector kindCol = impoundTable[Kind];
    CharacterVector nameCol = impoundTable[Name];
    CharacterVector genderCol = impoundTable[Gender];
    CharacterVector color1Col = impoundTable[Color1];
    CharacterVector color2Col = impoundTable[Color2];
    CharacterVector breed1Col = impoundTable[Breed1];
    CharacterVector breed2Col = impoundTable[Breed2];
    CharacterVector kennelCol = impoundTable[Kennel];
    
    CharacterVector spayNeuterCol = impoundTable[SpayNeuter];
    DatetimeVector intakeDateCol = impoundTable[IntakeDate];
    CharacterVector intakeTypeCol = impoundTable[IntakeType];
    CharacterVector intakeSubTypeCol = impoundTable[IntakeSubType];
    CharacterVector intakeConditionCol = impoundTable[IntakeCondition];
    CharacterVector intakeLocationCol = impoundTable[IntakeLocation];
    DatetimeVector outcomeDateCol = impoundTable[OutcomeDate];
    CharacterVector outcomeTypeCol = impoundTable[OutcomeType];
    CharacterVector outcomeSubTypeCol = impoundTable[OutcomeSubType];
    CharacterVector outcomeConditionCol = impoundTable[OutcomeCondition];
    
    // Add rows of animals and intakes and outcomes to the internal accumulator tables.
    
    for (int i = 0; i < numImpounds; ++i)
    {
        string animalId = as<string>(animalIdCol[i]);
        Datetime intakeDate = intakeDateCol[i];
        
        // Create an animal object from the animal information in the
        // impound record.

        AnimalRef animal = make_shared<Animal>(animalId, intakeDate);
        animal->SetKind(as<string>(kindCol[i]));
        animal->SetName(as<string>(nameCol[i]));
        animal->SetGender(as<string>(genderCol[i]));
        animal->SetColor1(as<string>(color1Col[i]));
        animal->SetColor2(as<string>(color2Col[i]));
        animal->SetBreed1(as<string>(breed1Col[i]));
        animal->SetBreed2(as<string>(breed2Col[i]));
        
        // Add or update the animal in the internal map.
        // The returned pointer is to the animal object stored in
        // the internal map.

        animal = AddAnimal(animal);
        
        // Create intake and outcome objects from the information in the
        // impound record. Add the intake-outcome pair to the internal map.

        IntakeRef intake = make_shared<Intake>();
        intake->SetKennel(as<string>(kennelCol[i]));
        intake->SetIntakeDate(intakeDateCol[i]);
        intake->SetIntakeType(as<string>(intakeTypeCol[i]));
        intake->SetIntakeSubType(as<string>(intakeSubTypeCol[i]));
        intake->SetIntakeCondition(as<string>(intakeConditionCol[i]));
        intake->SetIntakeLocation(as<string>(intakeLocationCol[i]));
        intake->SetIntakeSpayNeuter(as<string>(spayNeuterCol[i]));
        
        animal->AddIntake(intake);
        
        OutcomeRef outcome = make_shared<Outcome>();
        outcome->SetOutcomeDate(outcomeDateCol[i]);
        outcome->SetOutcomeType(as<string>(outcomeTypeCol[i]));
        outcome->SetOutcomeSubType(as<string>(outcomeSubTypeCol[i]));
        outcome->SetOutcomeCondition(as<string>(outcomeConditionCol[i]));
        
        animal->AddOutcome(outcome);
    }
}




/*
 *  Method: BuildImpoundTable
 *
 *      Builds the internal impound table by traversing the internal
 *      animal map and pairing-up intake and outcome objects.
 *      
 */
void DataFrameBuilder::BuildImpoundTable ()
{
    AnimalMap::const_iterator iter;
    
    // For each animal in the animal map.

    for (iter = mAnimalMap.begin(); iter != mAnimalMap.end(); ++iter)
    {
        AnimalRef animal = iter->second;
        
        // Order the intakes and outcomes by date and remove duplicates.

        animal->SortIntakes();
        animal->SortOutcomes();

        // Process both lists to pair-up intake events with subsequent outcome
        // events in order to create impound events (which are the rows
        // of the impound table).

        MergeAnimal(animal);
    }
}




/*
 *  Method: EmitSolitaryIntake
 *
 *      Add an intake event that is not paired with an outcome event to the
 *      internal impound table.
 *      
 *      Presumably this intake event corresponds to an animal that is still
 *      in the custody of the animal shelter.
 *      
 */
void DataFrameBuilder::EmitSolitaryIntake (const AnimalRef& animal, const IntakeRef& intake)
{
    //Rcout << animal->GetAnimalId() << " intake only" << endl;
    OutcomeRef outcome = make_shared<Outcome>();
    mImpoundTable.Append(animal, intake, outcome);
}




/*
 *  Method: EmitIntakeOutcome
 *
 *      Add a pair of intake and outcome events to the internal impound table.
 *      
 */
void DataFrameBuilder::EmitIntakeOutcome (const AnimalRef& animal, const IntakeRef& intake, const OutcomeRef& outcome)
{
    //Rcout << animal->GetAnimalId() << " outcome only" << endl;
    mImpoundTable.Append(animal, intake, outcome);
}




/*
 *  Method: EmitSolitaryOutcome
 *
 *      Add an outcome event that is not paired with an intake event to the
 *      internal impound table.
 *      
 *      This outcome event ought to correspond to an animal for which the
 *      intake event is missing from the data set being processed.
 *      
 */
void DataFrameBuilder::EmitSolitaryOutcome (const AnimalRef& animal, const OutcomeRef& outcome)
{
    //Rcout << animal->GetAnimalId() << " merged" << endl;
    IntakeRef intake = make_shared<Intake>();
    mImpoundTable.Append(animal, intake, outcome);
}




/*
 *  Method: MergeAnimal
 *
 *    Add impound records for the paired-up intake and outcome events of
 *    the specified animal.  
 *      
 */
void DataFrameBuilder::MergeAnimal (const AnimalRef& animal)
{
    int numIntakesRemaining = animal->GetNumIntakes();
    int numOutcomesRemaining = animal->GetNumOutcomes();
    int nextIntake = 0;
    int nextOutcome = 0;

    while (numIntakesRemaining > 0)
    {
        // Pair each intake with an outcome.
        // At the end there may be a solitary intake, meaning that the data
        // set ends with the animal in the custody of the shelter.
        
        IntakeRef intake = animal->GetIntakeAt(nextIntake);
        
        if (numOutcomesRemaining == 0)
        {
            // Intake(s) remaining but no more outcomes.

            if (numIntakesRemaining > 1)
            {
                // Discrepancy: multiple intakes are left over, not just one.
                // One or more late-date intakes is missing a matching outcome in the data set.
                
                Warning(animal, "Intake not matched with outcome.");

                // Take the most recent intake and discard the other(s).
                
                intake = animal->GetIntakeAt(animal->GetNumIntakes() - 1);

                // The solitary intake is the final event for the animal.

                EmitSolitaryIntake(animal, intake);
                numIntakesRemaining = 0;
                nextIntake = animal->GetNumIntakes();
            }
            else
            {
                // A single intake is left over and not paired with an outcome.
                // Add an impound record for the intake event.
                // The solitary intake is the final event for the animal.
                
                EmitSolitaryIntake(animal, intake);
                --numIntakesRemaining;
                ++nextIntake;
            }
        }
        else
        {
            // Try to pair the next intake event with the next outcome event.

            OutcomeRef outcome = animal->GetOutcomeAt(nextOutcome);
            --numOutcomesRemaining;
            ++nextOutcome;
            
            if (CompareByDay(outcome->GetOutcomeDate(), intake->GetIntakeDate()) == EarlierDay)
            {
                if (numIntakesRemaining == animal->GetNumIntakes())
                {
                    // When the next outcome is on an earlier day than the first intake,
                    // the outcome has to be solitary (i.e., the intake occurred before the
                    // first date in the data set). Emit the outcome by itself as its own
                    // impound event.
                
                    EmitSolitaryOutcome(animal, outcome);
                }
                else
                {
                    // Discrepancy: Unexpected outcome that is out of time order and
                    // does not pair with the next intake.
                    
                    Warning(animal, "Outcome out of order. Discarded.");
                }
            }
            else
            {
                // The next outcome is on the same or a later day than the next
                // intake. Pair the next intake event with the next outcome event and
                // emit the corresponding impound event.

                EmitIntakeOutcome(animal, intake, outcome);
                --numIntakesRemaining;
                ++nextIntake;
            }
        }
    }
    
    // Discrepancy: Process the outcome event(s) (if any) that are not paired
    // with an intake.
    
    if (numOutcomesRemaining > 0)
    {
        if (animal->GetNumIntakes() == 0)
        {
            // Okay to have left-over outcome event when there are no intake
            // events in the time period. This means the animal was taken up
            // prior to the first date in the data set.

            OutcomeRef outcome = animal->GetOutcomeAt(nextOutcome);
            
            EmitSolitaryOutcome(animal, outcome);
        }
        else
        {
            // Discrepancy: Extra outcome events are left over and not paired
            // with any intake event. Discard all of these extaneous events.

            Warning(animal, "Extra outcomes remaining at end.");
            //animal->DeepPrint(Rcout);
        }
    }
}




/*** EXPORTS *****************************************************************/

/*
 *  Method: sacMakeTables
 *
 *    Builds normalized Animal and Impound tables from the specified Sacramento
 *    open-data set.
 *      
 *    Returns an R list containing the two data frames.
 *    
 */
// [[Rcpp::export]]
List sacMakeTables (const DataFrame& impound)
{
    try
    {
        using namespace Col;
        DataFrameBuilder builder;

        // See if the input data frame has a record-source column.
        // If so, then the data frame contains CPRA records; otherwise,
        // the data frame contains open-data records. Open data records
        // have less information, and hence fewer columns, than CPRA
        // data records.

        bool cpraRecSource = impound.containsElementNamed(RecSource.c_str());
    
        if (cpraRecSource)
            builder.BuildFromSacCpraImpounds(impound);
        else
            builder.BuildFromSacOpenImpounds(impound);

        return List::create(Named("animal_data") = builder.GetAnimalDataFrame(),
                            Named("impound_data") = builder.GetImpoundDataFrame());
    }
    catch (string& message)
    {
        Rcout << "** Exception - " << message << endl;
        return NULL;
    }
}




/*
 *  Method: atxMakeTables
 *
 *    Builds normalized Animal and Impound tables from the specified Austin
 *    open-data intake and outcome data sets.
 *    
 *    Returns an R list containing the two data frames.
 *      
 */
// [[Rcpp::export]]
List atxMakeTables (const DataFrame& intake, const DataFrame& outcome)
{
    try
    {
        using namespace Col;
        DataFrameBuilder builder;
        
        builder.BuildFromAtxIntakesAndOutcomes(intake, outcome);

        return List::create(Named("animal_data") = builder.GetAnimalDataFrame(),
                            Named("impound_data") = builder.GetImpoundDataFrame());
    }
    catch (string& message)
    {
        Rcout << "** Exception - " << message << endl;
        return NULL;
    }
}
