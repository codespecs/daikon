/* rijndaeltest-ref.c   v2.0   August '99
 * Reference ANSI C code
 * authors: Paulo Barreto
 *          Vincent Rijmen, K.U.Leuven
 *
 * This code is placed in the public domain.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef TRACE_KAT_MCT
#include <time.h>
#endif /* ?TRACE_KAT_MCT */

#include "rijndael-api-ref.h"

#define SUBMITTER "Joan Daemen"

#ifndef MCT_ITERATIONS_OUTER
#define MCT_ITERATIONS_OUTER 400
#endif

#ifndef MCT_ITERATIONS_INNER
#define MCT_ITERATIONS_INNER 10000
#endif

#ifndef VARIABLE_TEST_SKIP_FACTOR
#define VARIABLE_TEST_SKIP_FACTOR
#endif

#ifndef TABLE_SIZE
#define TABLE_SIZE 64
#endif


static void blockPrint (FILE *fp, const BYTE *block, int blockBits, const char *tag)
{
	int i;

	fprintf (fp, "%s=", tag);
	for (i = 0; i < blockBits/8; i++) {
		fprintf (fp, "%02X", block[i]);
	}
	fprintf (fp, "\n");
	fflush (fp);
} /* blockPrint */


static void HexToBin (BYTE *binBlock, const BYTE *hexBlock, int blockLen)
{
	int j = 0, t;
	while (blockLen > 0) {
		t = *hexBlock++;
		if ((t >= '0') && (t <= '9')) j = (t - '0') << 4;
		else if ((t >= 'a') && (t <= 'f')) j = (t - 'a' + 10) << 4;
		else if ((t >= 'A') && (t <= 'F')) j = (t - 'A' + 10) << 4;

		t = *hexBlock++;
		if ((t >= '0') && (t <= '9')) j ^= (t - '0');
		else if ((t >= 'a') && (t <= 'f')) j ^= (t - 'a' + 10);
		else if ((t >= 'A') && (t <= 'F')) j ^= (t - 'A' + 10);

		*binBlock++ = j;
		blockLen -= 8;
	}
} /* HexToBin */


static void rijndaelVKKAT (FILE *fp, int keyLength, int blockLength)
{
	int i, j, r;
	BYTE block[4*MAXBC];
	BYTE keyMaterial[320];
	BYTE byteVal = (BYTE)'8';
	keyInstance keyInst;
	cipherInstance cipherInst;

#ifdef TRACE_KAT_MCT
	printf ("Executing Variable-Key KAT (key %d): ", keyLength);
	fflush (stdout);
#endif /* ?TRACE_KAT_MCT */
	fprintf (fp,
		"\n"
		"==========\n"
		"\n"
		"KEYSIZE=%d\n"
		"\n", keyLength);
	fflush (fp);
	memset (block, 0, blockLength/8);
	blockPrint (fp, block, blockLength, "PT");
	memset (keyMaterial, 0, sizeof (keyMaterial));
	memset (keyMaterial, '0', keyLength/4);
	for (i = 0; i < keyLength/VARIABLE_TEST_SKIP_FACTOR; i++) {
		keyMaterial[i/4] = byteVal; /* set only the i-th bit of the i-th test key */
		keyInst.blockLen = blockLength;
		r = makeKey(&keyInst, DIR_ENCRYPT, keyLength, keyMaterial);
		if (TRUE != r) {
			fprintf(stderr,"makeKey error %d\n",r);
			exit(-1);
		}
		fprintf (fp, "\nI=%d\n", i+1);
		fprintf (fp, "KEY=%s\n", keyMaterial);
		memset (block, 0, blockLength/8);
		cipherInst.blockLen = blockLength;
		r = cipherInit (&cipherInst, MODE_ECB, NULL);
		if (TRUE != r) {
			fprintf(stderr,"cipherInit error %d\n",r);
			exit(-1);
		}
		r = blockEncrypt(&cipherInst, &keyInst, block, blockLength, block);
		if (blockLength != r) {
			fprintf(stderr,"blockEncrypt error %d\n",r);
			exit(-1);
		}
		blockPrint (fp, block, blockLength, "CT");
		/* now check decryption: */
		keyInst.blockLen = blockLength;
		makeKey(&keyInst, DIR_DECRYPT, keyLength, keyMaterial);
		blockDecrypt(&cipherInst, &keyInst, block, blockLength, block);
		for (j = 0; j < blockLength/8; j++) {
			assert (block[j] == 0);
		}
		/* undo changes for the next iteration: */
		keyMaterial[i/4] = (BYTE)'0';
		byteVal =
			(byteVal == '8') ?	'4' :
			(byteVal == '4') ?	'2' :
			(byteVal == '2') ?	'1' :
		/*	(byteVal == '1') */	'8';
	}
	assert (byteVal == (BYTE)'8');

#ifdef TRACE_KAT_MCT
	printf (" done.\n");
#endif /* ?TRACE_KAT_MCT */
} /* rijndaelVKKAT */


static void rijndaelVTKAT (FILE *fp, int keyLength, int blockLength)
{
	int i;
	BYTE block[4*MAXBC];
	BYTE keyMaterial[320];
	keyInstance keyInst;
	cipherInstance cipherInst;

#ifdef TRACE_KAT_MCT
	printf ("Executing Variable-Text KAT (key %d): ", keyLength);
	fflush (stdout);
#endif /* ?TRACE_KAT_MCT */
	fprintf (fp,
		"\n"
		"==========\n"
		"\n"
		"KEYSIZE=%d\n"
		"\n", keyLength);
	fflush (fp);
	memset (keyMaterial, 0, sizeof (keyMaterial));
	memset (keyMaterial, '0', keyLength/4);
	keyInst.blockLen = blockLength;
	makeKey(&keyInst, DIR_ENCRYPT, keyLength, keyMaterial);
	fprintf (fp, "KEY=%s\n", keyMaterial);
	for (i = 0; i < blockLength/VARIABLE_TEST_SKIP_FACTOR; i++) {
		memset (block, 0, blockLength/8);
		block[i/8] |= 1 << (7 - i%8); /* set only the i-th bit of the i-th test block */
		fprintf (fp, "\nI=%d\n", i+1);
		blockPrint (fp, block, blockLength, "PT");
		cipherInst.blockLen = blockLength;
		cipherInit (&cipherInst, MODE_ECB, NULL);
		blockEncrypt(&cipherInst, &keyInst, block, blockLength, block);
		blockPrint (fp, block, blockLength, "CT");
	}
#ifdef TRACE_KAT_MCT
	printf (" done.\n");
#endif /* ?TRACE_KAT_MCT */
}


static void rijndaelTKAT (FILE *fp, int blockLength, int keyLength, FILE *in)
{
	int i, j;
	unsigned int s;
	BYTE block[4*MAXBC], block2[4*MAXBC];
	BYTE keyMaterial[320];
	keyInstance keyInst;
	cipherInstance cipherInst;

#ifdef TRACE_KAT_MCT
	printf ("Executing Tables KAT (key %d): ", keyLength);
	fflush (stdout);
#endif /* ?TRACE_KAT_MCT */
	fprintf (fp,
		"\n"
		"==========\n"
		"\n"
		"KEYSIZE=%d\n"
		"\n", keyLength);
	fflush (fp);

	memset (keyMaterial, 0, sizeof (keyMaterial));

	for(i = 0; i < TABLE_SIZE; i++) {
		fprintf (fp, "\nI=%d\n", i+1);
		for(j = 0; j < keyLength/4; j++) {
			fscanf(in,"%c",&keyMaterial[j]);
		}
		keyInst.blockLen = blockLength;
		makeKey(&keyInst, DIR_ENCRYPT, keyLength, keyMaterial);

		fprintf (fp, "KEY=%s\n", keyMaterial);

		for(j = 0; j < blockLength/8; j++) {
			fscanf(in,"%02x",&s);
			block[j] = s;
		}
		fscanf(in,"%c",&s);
		fscanf(in,"%c",&s);
		blockPrint (fp, block, blockLength, "PT");
		cipherInst.blockLen = blockLength;
		cipherInit (&cipherInst, MODE_ECB, NULL);
		blockEncrypt(&cipherInst, &keyInst, block, blockLength, block2);
		blockPrint (fp, block2, blockLength, "CT");
	}
	for(i = TABLE_SIZE; i < 2*TABLE_SIZE; i++) {
		fprintf (fp, "\nI=%d\n", i+1);
		for(j = 0; j < keyLength/4; j++) {
			fscanf(in,"%c",&keyMaterial[j]);
		}
		keyInst.blockLen = blockLength;
		makeKey(&keyInst, DIR_DECRYPT, keyLength, keyMaterial);

		fprintf (fp, "KEY=%s\n", keyMaterial);

		for(j = 0; j < blockLength/8; j++) {
			fscanf(in,"%02x",&s);
			block[j] = s;
		}
		fscanf(in,"%c",&s);
		fscanf(in,"%c",&s);
		cipherInst.blockLen = blockLength;
		cipherInit (&cipherInst, MODE_ECB, NULL);
		blockDecrypt(&cipherInst, &keyInst, block, blockLength, block2);
		blockPrint (fp, block2, blockLength, "PT");
		blockPrint (fp, block, blockLength, "CT");
	}

#ifdef TRACE_KAT_MCT
	printf (" done.\n");
#endif /* ?TRACE_KAT_MCT */
}

static void rijndaelIVKAT (FILE *fp, int keyLength, int blockLength)
{
	int i, ROUNDS;
	BYTE block[4*MAXBC], block2[4*MAXBC];
	BYTE keyMaterial[320];
	keyInstance keyInst;
	cipherInstance cipherInst;
	char format[10];
#ifdef TRACE_KAT_MCT
	printf ("Executing Intermediate value KAT (key %d): ", keyLength);
	fflush (stdout);
#endif /* ?TRACE_KAT_MCT */

	switch (keyLength >= blockLength ? keyLength : blockLength) {
	case 128: ROUNDS = 10; break;
	case 192: ROUNDS = 12; break;
	case 256: ROUNDS = 14; break;
	default : return; /* this cannot happen */
	}

	fprintf (fp,
		"\n"
		"==========\n"
		"\n"
		"KEYSIZE=%d\n",
		keyLength);
	fflush (fp);
	memset (keyMaterial, 0, sizeof (keyMaterial));
	for (i = 0; i < keyLength/8; i++) {
		sprintf (&keyMaterial[2*i], "%02X", i);
	}
	keyInst.blockLen = blockLength;
	makeKey(&keyInst, DIR_ENCRYPT, keyLength, keyMaterial);
	fprintf (fp, "KEY=%s\n", keyMaterial);
	fprintf (fp, "\nIntermediate Ciphertext Values (Encryption)\n\n");
	for (i = 0; i < blockLength/8; i++) {
		block[i] = i;
	}
	blockPrint (fp, block, blockLength, "PT");
	cipherInst.blockLen = blockLength;
	cipherInit (&cipherInst, MODE_ECB, NULL);
	for(i = 1; i < ROUNDS; i++) {
		cipherUpdateRounds (&cipherInst, &keyInst, block, blockLength/8, block2, i);
		sprintf(format,"CT%d",i);
		blockPrint (fp, block2, blockLength, format);
	}
	cipherUpdateRounds (&cipherInst, &keyInst, block, blockLength, block2, ROUNDS);
	blockPrint (fp, block2, blockLength, "CT");

	keyInst.blockLen = blockLength;
	makeKey(&keyInst, DIR_DECRYPT, keyLength, keyMaterial);
	fprintf (fp, "\nIntermediate Ciphertext Values (Decryption)\n\n");
	blockPrint (fp, block2, blockLength, "CT");
	cipherInst.blockLen = blockLength;
	cipherInit (&cipherInst, MODE_ECB, NULL);
	for(i = 1; i < ROUNDS; i++) {
		cipherUpdateRounds (&cipherInst, &keyInst, block2, blockLength,block, ROUNDS-i);
		sprintf(format,"PT%d",i);
		blockPrint (fp, block, blockLength, format);
	}
	cipherUpdateRounds (&cipherInst, &keyInst, block2, blockLength, block, 0);
	blockPrint (fp, block, blockLength, "PT");

#ifdef TRACE_KAT_MCT
	printf (" done.\n");
#endif
}

static void makeKATs (const char *vkFile, const char *vtFile, const char *tblFile,
                      const char *ivFile)
{
	FILE *fp, *fp2;

	/* prepare Variable Key Known Answer Tests: */
	fp = fopen (vkFile, "w");
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Electronic Codebook (ECB) Mode\n"
		"Variable Key Known Answer Tests\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		vkFile,SUBMITTER);
	fflush (fp);

	rijndaelVKKAT (fp, 128, BITSPERBLOCK); /* test for 128-bit key */
	rijndaelVKKAT (fp, 192, BITSPERBLOCK); /* test for 192-bit key */
	rijndaelVKKAT (fp, 256, BITSPERBLOCK); /* test for 256-bit key */

	fprintf (fp,
		"\n"
		"==========\n");
	fclose (fp);

	/* prepare Variable Text Known Answer Tests: */
	fp = fopen (vtFile, "w");
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Electronic Codebook (ECB) Mode\n"
		"Variable Text Known Answer Tests\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		vtFile,SUBMITTER);
	fflush (fp);

	rijndaelVTKAT (fp, 128, BITSPERBLOCK);
	rijndaelVTKAT (fp, 192, BITSPERBLOCK);
	rijndaelVTKAT (fp, 256, BITSPERBLOCK);

	fprintf (fp,
		"\n"
		"==========\n");
	fclose (fp);

	/* prepare Tables Known Answer Tests: */
	fp = fopen (tblFile, "w");
	fprintf (fp,
	        "/* Description of what tables are tested:\n"
		"   The provided implementations each use a different set of tables\n"
		"    - Java implementation: uses no tables\n"
		"    - reference C implementation: uses Logtable, Alogtable, S, Si, rcon\n"
		"    - fast C implementation: uses Logtable, Alogtable,  rcon\n"
		"        and additionally, T1, T2, T3, T4, T5, T6, T7, T8\n"
		"        and (for the inverse key schedule only) U1, U2, U3, U4.\n"
		"   All these tables are tested.\n"
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Electronic Codebook (ECB) Mode\n"
		"Tables Known Answer Tests\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		tblFile,SUBMITTER);
	fflush (fp);


	if (NULL != (fp2 = fopen("table.128","r"))) {
		rijndaelTKAT (fp, 128, 128,fp2);
		fclose(fp2);
	} else {
		printf("Table Known Answer test expects file table.128\n");
		fclose(fp);
		exit (EXIT_FAILURE);
	}
	if (NULL != (fp2 = fopen("table.192","r"))) {
		rijndaelTKAT (fp, 128, 192,fp2);
		fclose(fp2);
	} else {
		printf("Table Known Answer test expects file table.192\n");
		fclose(fp);
		exit (EXIT_FAILURE);
	}
	if (NULL != (fp2 = fopen("table.256","r"))) {
		rijndaelTKAT (fp, 128, 256,fp2);
		fclose(fp2);
	} else {
		printf("Table Known Answer test expects file table.192\n");
		fclose(fp);
		exit (EXIT_FAILURE);
	}

	fprintf (fp,
		"\n"
		"==========\n");
	fclose (fp);

	/* prepare Intermediate Values Known Answer Tests: */
	fp = fopen (ivFile, "w");
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Electronic Codebook (ECB) Mode\n"
		"Intermediate Value Known Answer Tests\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		ivFile,SUBMITTER);
	fflush (fp);

	rijndaelIVKAT (fp, 128, BITSPERBLOCK);
	rijndaelIVKAT (fp, 192, BITSPERBLOCK);
	rijndaelIVKAT (fp, 256, BITSPERBLOCK);

	fprintf (fp,
		"\n"
		"==========\n");
	fclose (fp);

}

static void rijndaelECB_MCT (FILE *fp, const char *initKey, int keyLength,
	const char *initBlock, int blockLength, BYTE direction)
{
	int i, j;
	BYTE inBlock[4*MAXBC], outBlock[4*MAXBC], binKey[4*MAXKC];
	BYTE keyMaterial[320];
	keyInstance keyInst;
	cipherInstance cipherInst;

#ifdef TRACE_KAT_MCT
	int width = 0;
	clock_t elapsed = -clock();
	printf ("Executing ECB MCT (%s, key %d): ",
		direction == DIR_ENCRYPT ? "ENCRYPT" : "DECRYPT", keyLength);
	fflush (stdout);
#endif /* ?TRACE_KAT_MCT */
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"KEYSIZE=%d\n", keyLength);
	fflush (fp);
	HexToBin (outBlock, initBlock, blockLength);
	HexToBin (binKey, initKey, keyLength);
	for (i = 0; i < MCT_ITERATIONS_OUTER; i++) {
#ifdef TRACE_KAT_MCT
        while (width-- > 0) putchar ('\b'); width = printf ("%d", i); fflush (stdout);
#endif /* ?TRACE_KAT_MCT */
		fprintf (fp, "\nI=%d\n", i);
		/* prepare key: */
		for (j = 0; j < keyLength/8; j++) {
			sprintf (&keyMaterial[2*j], "%02X", binKey[j]);
		}
		keyMaterial[keyLength/4] = 0;
		fprintf (fp, "KEY=%s\n", keyMaterial);
		keyInst.blockLen = blockLength;
		makeKey(&keyInst, direction, keyLength, keyMaterial);
		/* do encryption/decryption: */
		blockPrint (fp, outBlock, blockLength, direction == DIR_ENCRYPT ? "PT" : "CT");
		cipherInst.blockLen = blockLength;
		cipherInit (&cipherInst, MODE_ECB, NULL);
		if (direction == DIR_ENCRYPT) {
			for (j = 0; j < MCT_ITERATIONS_INNER; j++) {
				memcpy (inBlock, outBlock, blockLength/8);
				blockEncrypt(&cipherInst, &keyInst, inBlock, blockLength, outBlock);
			}
		} else {
			for (j = 0; j < MCT_ITERATIONS_INNER; j++) {
				memcpy (inBlock, outBlock, blockLength/8);
				blockDecrypt(&cipherInst, &keyInst, inBlock, blockLength, outBlock);
			}
		}
		blockPrint (fp, outBlock, blockLength, direction == DIR_ENCRYPT ? "CT" : "PT");
		/* prepare new key: */
		switch (keyLength) {
		case 128:
			for (j = 0; j < 128/8; j++) {
				binKey[j] ^= outBlock[j];
			}
			break;
		case 192:
			for (j = 0; j < 64/8; j++) {
				binKey[j] ^= inBlock[j + 64/8];
			}
			for (j = 0; j < 128/8; j++) {
				binKey[j + 64/8] ^= outBlock[j];
			}
			break;
		case 256:
			for (j = 0; j < 128/8; j++) {
				binKey[j] ^= inBlock[j];
			}
			for (j = 0; j < 128/8; j++) {
				binKey[j + 128/8] ^= outBlock[j];
			}
			break;
		}
	}
#ifdef TRACE_KAT_MCT
	elapsed += clock();
	printf (" done (%.1f s).\n", (float)elapsed/CLOCKS_PER_SEC);
#endif /* ?TRACE_KAT_MCT */
} /* rijndaelECB_MCT */


static void rijndaelCBC_MCT (FILE *fp, const char *initKey, int keyLength,
	const char *initIV, const char *initBlock, int blockLength, BYTE direction)
{
	int i, j, r, t;
	BYTE inBlock[256/8], outBlock[256/8], binKey[256/8], cv[256/8];
	BYTE keyMaterial[320];
	BYTE iv[64+1];
	keyInstance keyInst;
	cipherInstance cipherInst;

#ifdef TRACE_KAT_MCT
	int width = 0;
	clock_t elapsed = -clock();
	printf ("Executing CBC MCT (%s, key %d): ",
		direction == DIR_ENCRYPT ? "ENCRYPT" : "DECRYPT", keyLength);
	fflush (stdout);
#endif /* ?TRACE_KAT_MCT */
	fprintf (fp,
		"\n"
		"==========\n"
		"\n"
		"KEYSIZE=%d\n", keyLength);
	fflush (fp);
	HexToBin (inBlock, initBlock, blockLength); /* this is either PT0 or CT0 */
	HexToBin (cv, initIV, blockLength);
	HexToBin (binKey, initKey, keyLength);
	for (i = 0; i < MCT_ITERATIONS_OUTER; i++) {
#ifdef TRACE_KAT_MCT
        while (width-- > 0) putchar ('\b'); width = printf ("%d", i); fflush (stdout);
#endif /* ?TRACE_KAT_MCT */
		fprintf (fp, "\nI=%d\n", i);
		/* prepare key: */
		for (j = 0; j < keyLength/8; j++) {
			sprintf (&keyMaterial[2*j], "%02X", binKey[j]);
		}
		keyMaterial[keyLength/4] = 0;
		fprintf (fp, "KEY=%s\n", keyMaterial);
		keyInst.blockLen = blockLength;
		r = makeKey(&keyInst, direction, keyLength, keyMaterial);
		if (TRUE != r) {
			fprintf(stderr,"makeKey error %d\n",r);
			exit(-1);
		}
		/* do encryption/decryption: */
		blockPrint (fp, cv, blockLength, "IV");
		blockPrint (fp, inBlock, blockLength, direction == DIR_ENCRYPT ? "PT" : "CT");
		if (direction == DIR_ENCRYPT) {
			for (j = 0; j < MCT_ITERATIONS_INNER; j++) {
				for(t = 0; t < blockLength/8; t++) {
					sprintf(iv+2*t,"%02x",cv[t]);
				}
				cipherInst.blockLen = blockLength;
				r = cipherInit (&cipherInst, MODE_CBC, iv);
				if (TRUE != r) {
					fprintf(stderr,"cipherInit error %d\n",r);
					exit(-1);
				}
				r = blockEncrypt(&cipherInst, &keyInst, inBlock, blockLength, outBlock);
				if (blockLength != r) {
					fprintf(stderr,"blockEncrypt error %d\n",r);
					exit(-1);
				}
				memcpy (inBlock, cv, blockLength/8);
				memcpy (cv, outBlock, blockLength/8);
			}
		} else {
			for (j = 0; j < MCT_ITERATIONS_INNER; j++) {
				for(t = 0; t < blockLength/8; t++) {
					sprintf(iv+2*t,"%02x",cv[t]);
				}
				cipherInst.blockLen = blockLength;
				cipherInit (&cipherInst, MODE_CBC, iv);
				blockDecrypt(&cipherInst, &keyInst, inBlock, blockLength, outBlock);
				memcpy (cv, inBlock, blockLength/8);
				memcpy (inBlock, outBlock, blockLength/8);
			}
		}
		blockPrint (fp, outBlock, blockLength, direction == DIR_ENCRYPT ? "CT" : "PT");
		/* prepare new key: */
		switch (keyLength) {
		case 128:
			for (j = 0; j < 128/8; j++) {
				binKey[j] ^= outBlock[j];
			}
			break;
		case 192:
			for (j = 0; j < 64/8; j++) {
				if (direction == DIR_ENCRYPT)
					binKey[j] ^= inBlock[j + 64/8];
				else
					binKey[j] ^= cv[j + 64/8];
			}
			for (j = 0; j < 128/8; j++) {
				binKey[j + 64/8] ^= outBlock[j];
			}
			break;
		case 256:
			for (j = 0; j < 128/8; j++) {
				if (direction == DIR_ENCRYPT)
					binKey[j] ^= inBlock[j];
				else
					binKey[j] ^= cv[j];
			}
			for (j = 0; j < 128/8; j++) {
				binKey[j + 128/8] ^= outBlock[j];
			}
			break;
		}
	}
#ifdef TRACE_KAT_MCT
	elapsed += clock();
	printf (" done (%.1f s).\n", (float)elapsed/CLOCKS_PER_SEC);
#endif /* ?TRACE_KAT_MCT */
} /* rijndaelCBC_MCT */


static void makeMCTs (const char *ecbEncryptionFile, const char *ecbDecryptionFile,
	const char *cbcEncryptionFile, const char *cbcDecryptionFile)
{
	FILE *fp;

	/* prepare ECB Encryption Monte Carlo Tests: */
	fp = fopen (ecbEncryptionFile, "w");
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Electronic Codebook (ECB) Mode - ENCRYPTION\n"
		"Monte Carlo Test\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		ecbEncryptionFile,SUBMITTER);
	fflush (fp);

	rijndaelECB_MCT (fp,
		"00000000000000000000000000000000", 128,
		"00000000000000000000000000000000", BITSPERBLOCK, DIR_ENCRYPT);

	rijndaelECB_MCT (fp,
		"000000000000000000000000000000000000000000000000", 192,
		"00000000000000000000000000000000", BITSPERBLOCK, DIR_ENCRYPT);

	rijndaelECB_MCT (fp,
		"0000000000000000000000000000000000000000000000000000000000000000", 256,
		"00000000000000000000000000000000", BITSPERBLOCK, DIR_ENCRYPT);

	fprintf (fp,
		"\n"
		"===========\n");
	fclose (fp);

	/* prepare ECB Decryption Monte Carlo Tests: */
	fp = fopen (ecbDecryptionFile, "w");
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Electronic Codebook (ECB) Mode - DECRYPTION\n"
		"Monte Carlo Test\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		ecbDecryptionFile,SUBMITTER);
	fflush (fp);

	rijndaelECB_MCT (fp,
		"00000000000000000000000000000000", 128,
		"00000000000000000000000000000000", BITSPERBLOCK, DIR_DECRYPT);

	rijndaelECB_MCT (fp,
		"000000000000000000000000000000000000000000000000", 192,
		"00000000000000000000000000000000", BITSPERBLOCK, DIR_DECRYPT);

	rijndaelECB_MCT (fp,
		"0000000000000000000000000000000000000000000000000000000000000000", 256,
		"00000000000000000000000000000000", BITSPERBLOCK, DIR_DECRYPT);

	fprintf (fp,
		"\n"
		"===========\n");
	fclose (fp);

	/* prepare CBC Encryption Monte Carlo Tests: */
	fp = fopen (cbcEncryptionFile, "w");
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Cipher Block Chaining (CBC) Mode - ENCRYPTION\n"
		"Monte Carlo Test\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		cbcEncryptionFile,SUBMITTER);
	fflush (fp);

	rijndaelCBC_MCT (fp,
		"00000000000000000000000000000000", 128,
		"00000000000000000000000000000000", "00000000000000000000000000000000", BITSPERBLOCK,
		DIR_ENCRYPT);

	rijndaelCBC_MCT (fp,
		"000000000000000000000000000000000000000000000000", 192,
		"00000000000000000000000000000000", "00000000000000000000000000000000", BITSPERBLOCK,
		DIR_ENCRYPT);

	rijndaelCBC_MCT (fp,
		"0000000000000000000000000000000000000000000000000000000000000000", 256,
		"00000000000000000000000000000000", "00000000000000000000000000000000", BITSPERBLOCK,
		DIR_ENCRYPT);

	fprintf (fp,
		"\n"
		"===========\n");
	fclose (fp);

	/* prepare CBC Decryption Monte Carlo Tests: */
	fp = fopen (cbcDecryptionFile, "w");
	fprintf (fp,
		"\n"
		"=========================\n"
		"\n"
		"FILENAME:  \"%s\"\n"
		"\n"
		"Cipher Block Chaining (CBC) Mode - DECRYPTION\n"
		"Monte Carlo Test\n"
		"\n"
		"Algorithm Name: Rijndael\n"
		"Principal Submitter: %s\n",
		cbcDecryptionFile,SUBMITTER);
	fflush (fp);

	rijndaelCBC_MCT (fp,
		"00000000000000000000000000000000", 128,
		"00000000000000000000000000000000", "00000000000000000000000000000000", BITSPERBLOCK,
		DIR_DECRYPT);

	rijndaelCBC_MCT (fp,
		"000000000000000000000000000000000000000000000000", 192,
		"00000000000000000000000000000000", "00000000000000000000000000000000", BITSPERBLOCK,
		DIR_DECRYPT);

	rijndaelCBC_MCT (fp,
		"0000000000000000000000000000000000000000000000000000000000000000", 256,
		"00000000000000000000000000000000", "00000000000000000000000000000000", BITSPERBLOCK,
		DIR_DECRYPT);

	fprintf (fp,
		"\n"
		"===========\n");
	fclose (fp);

} /* makeMCTs */


int main (void)
{
	makeKATs ("ecb_vk.txt", "ecb_vt.txt", "ecb_tbl.txt", "ecb_iv.txt");
	makeMCTs ("ecb_e_m.txt", "ecb_d_m.txt", "cbc_e_m.txt", "cbc_d_m.txt");

	return 0;
}
