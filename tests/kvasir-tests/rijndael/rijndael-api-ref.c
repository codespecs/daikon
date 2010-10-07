/* rijndael-api-ref.c   v2.1   April 2000
 * Reference ANSI C code
 * authors: v2.0 Paulo Barreto
 *               Vincent Rijmen, K.U.Leuven
 *          v2.1 Vincent Rijmen, K.U.Leuven
 *
 * This code is placed in the public domain.
 */
#include <stdlib.h>
#include <string.h>

#include "rijndael-alg-ref.h"
#include "rijndael-api-ref.h"


int makeKey(keyInstance *key, BYTE direction, int keyLen, char *keyMaterial)
{
	word8 k[4][MAXKC];
	int i, j, t;
	
	if (key == NULL) {
		return BAD_KEY_INSTANCE;
	}

	if ((direction == DIR_ENCRYPT) || (direction == DIR_DECRYPT)) {
		key->direction = direction;
	} else {
		return BAD_KEY_DIR;
	}

	if ((keyLen == 128) || (keyLen == 192) || (keyLen == 256)) { 
		key->keyLen = keyLen;
	} else {
		return BAD_KEY_MAT;
	}

	if ( keyMaterial ) {
		strncpy(key->keyMaterial, keyMaterial, keyLen/4);
	}

	/* initialize key schedule: */ 
 	for(i = 0; i < key->keyLen/8; i++) {
		t = key->keyMaterial[2*i];
		if ((t >= '0') && (t <= '9')) j = (t - '0') << 4;
		else if ((t >= 'a') && (t <= 'f')) j = (t - 'a' + 10) << 4; 
		else if ((t >= 'A') && (t <= 'F')) j = (t - 'A' + 10) << 4; 
		else return BAD_KEY_MAT;
		
		t = key->keyMaterial[2*i+1];
		if ((t >= '0') && (t <= '9')) j ^= (t - '0');
		else if ((t >= 'a') && (t <= 'f')) j ^= (t - 'a' + 10); 
		else if ((t >= 'A') && (t <= 'F')) j ^= (t - 'A' + 10); 
		else return BAD_KEY_MAT;
		
		k[i % 4][i / 4] = (word8) j; 
	}	
	
	rijndaelKeySched (k, key->keyLen, key->blockLen, key->keySched);
	
	return TRUE;
}

int cipherInit(cipherInstance *cipher, BYTE mode, char *IV)
{
	int i, j, t;
	
	if ((mode == MODE_ECB) || (mode == MODE_CBC) || (mode == MODE_CFB1)) {
		cipher->mode = mode;
	} else {
		return BAD_CIPHER_MODE;
	}
	
	if (IV != NULL) {
 		for(i = 0; i < cipher->blockLen/8; i++) {		
			t = IV[2*i];
			if ((t >= '0') && (t <= '9')) j = (t - '0') << 4;
			else if ((t >= 'a') && (t <= 'f')) j = (t - 'a' + 10) << 4; 
			else if ((t >= 'A') && (t <= 'F')) j = (t - 'A' + 10) << 4; 
			else return BAD_CIPHER_INSTANCE;
		
			t = IV[2*i+1];
			if ((t >= '0') && (t <= '9')) j ^= (t - '0');
			else if ((t >= 'a') && (t <= 'f')) j ^= (t - 'a' + 10); 
			else if ((t >= 'A') && (t <= 'F')) j ^= (t - 'A' + 10); 
			else return BAD_CIPHER_INSTANCE;
			
			cipher->IV[i] = (BYTE) j;
		} 
	}

	return TRUE;
}


int blockEncrypt(cipherInstance *cipher,
	keyInstance *key, BYTE *input, int inputLen, BYTE *outBuffer)
{
	int i, j, t, numBlocks;
	word8 block[4][MAXBC];

	
        /* check parameter consistency: */
        if (key == NULL ||
                key->direction != DIR_ENCRYPT ||
                (key->keyLen != 128 && key->keyLen != 192 && key->keyLen != 256)) {
                return BAD_KEY_MAT;
        }
        if (cipher == NULL ||
                (cipher->mode != MODE_ECB && cipher->mode != MODE_CBC && cipher->mode != MODE_CFB1) ||
                (cipher->blockLen != 128 && cipher->blockLen != 192 && cipher->blockLen != 256)) {
                return BAD_CIPHER_STATE;
        }


	numBlocks = inputLen/cipher->blockLen;
	
	switch (cipher->mode) {
	case MODE_ECB: 
		for (i = 0; i < numBlocks; i++) {
			for (j = 0; j < cipher->blockLen/32; j++) {
				for(t = 0; t < 4; t++)
				/* parse input stream into rectangular array */
					block[t][j] = input[cipher->blockLen/8*i+4*j+t] & 0xFF;
			}
			rijndaelEncrypt (block, key->keyLen, cipher->blockLen, key->keySched);
			for (j = 0; j < cipher->blockLen/32; j++) {
				/* parse rectangular array into output ciphertext bytes */
				for(t = 0; t < 4; t++)
					outBuffer[cipher->blockLen/8*i+4*j+t] = (BYTE) block[t][j];
			}
		}
		break;
		
	case MODE_CBC:
		for (j = 0; j < cipher->blockLen/32; j++) {
			for(t = 0; t < 4; t++)
			/* parse initial value into rectangular array */
					block[t][j] = cipher->IV[t+4*j] & 0xFF;
			}
		for (i = 0; i < numBlocks; i++) {
			for (j = 0; j < cipher->blockLen/32; j++) {
				for(t = 0; t < 4; t++)
				/* parse input stream into rectangular array and exor with 
				   IV or the previous ciphertext */
					block[t][j] ^= input[cipher->blockLen/8*i+4*j+t] & 0xFF;
			}
			rijndaelEncrypt (block, key->keyLen, cipher->blockLen, key->keySched);
			for (j = 0; j < cipher->blockLen/32; j++) {
				/* parse rectangular array into output ciphertext bytes */
				for(t = 0; t < 4; t++)
					outBuffer[cipher->blockLen/8*i+4*j+t] = (BYTE) block[t][j];
			}
		}
		break;
	
	default: return BAD_CIPHER_STATE;
	}
	
	return numBlocks*cipher->blockLen;
}

int blockDecrypt(cipherInstance *cipher,
	keyInstance *key, BYTE *input, int inputLen, BYTE *outBuffer)
{
	int i, j, t, numBlocks;
	word8 block[4][MAXBC];

	if (cipher == NULL ||
		key == NULL ||
		key->direction == DIR_ENCRYPT ||
		cipher->blockLen != key->blockLen) {
		return BAD_CIPHER_STATE;
	}

        /* check parameter consistency: */
        if (key == NULL ||
                key->direction != DIR_DECRYPT ||
                (key->keyLen != 128 && key->keyLen != 192 && key->keyLen != 256)) {
                return BAD_KEY_MAT;
        }
        if (cipher == NULL ||
                (cipher->mode != MODE_ECB && cipher->mode != MODE_CBC && cipher->mode != MODE_CFB1) ||
                (cipher->blockLen != 128 && cipher->blockLen != 192 && cipher->blockLen != 256)) {
                return BAD_CIPHER_STATE;
        }
	

	numBlocks = inputLen/cipher->blockLen;
	
	switch (cipher->mode) {
	case MODE_ECB: 
		for (i = 0; i < numBlocks; i++) {
			for (j = 0; j < cipher->blockLen/32; j++) {
				for(t = 0; t < 4; t++)
				/* parse input stream into rectangular array */
					block[t][j] = input[cipher->blockLen/8*i+4*j+t] & 0xFF;
			}
			rijndaelDecrypt (block, key->keyLen, cipher->blockLen, key->keySched);
			for (j = 0; j < cipher->blockLen/32; j++) {
				/* parse rectangular array into output ciphertext bytes */
				for(t = 0; t < 4; t++)
					outBuffer[cipher->blockLen/8*i+4*j+t] = (BYTE) block[t][j];
			}
		}
		break;
		
	case MODE_CBC:
		/* first block */
		for (j = 0; j < cipher->blockLen/32; j++) {
			for(t = 0; t < 4; t++)
			/* parse input stream into rectangular array */
				block[t][j] = input[4*j+t] & 0xFF;
		}
		rijndaelDecrypt (block, key->keyLen, cipher->blockLen, key->keySched);
		
		for (j = 0; j < cipher->blockLen/32; j++) {
			/* exor the IV and parse rectangular array into output ciphertext bytes */
			for(t = 0; t < 4; t++)
				outBuffer[4*j+t] = (BYTE) (block[t][j] ^ cipher->IV[t+4*j]);
		}
		
		/* next blocks */
		for (i = 1; i < numBlocks; i++) {
			for (j = 0; j < cipher->blockLen/32; j++) {
				for(t = 0; t < 4; t++)
				/* parse input stream into rectangular array */
					block[t][j] = input[cipher->blockLen/8*i+4*j+t] & 0xFF;
			}
			rijndaelDecrypt (block, key->keyLen, cipher->blockLen, key->keySched);
			
			for (j = 0; j < cipher->blockLen/32; j++) {
				/* exor previous ciphertext block and parse rectangular array 
				       into output ciphertext bytes */
				for(t = 0; t < 4; t++)
					outBuffer[cipher->blockLen/8*i+4*j+t] = (BYTE) (block[t][j] ^ 
						input[cipher->blockLen/8*i+4*j+t-4*cipher->blockLen/32]);
			}
		}
		break;
	
	default: return BAD_CIPHER_STATE;
	}
	
	return numBlocks*cipher->blockLen;
}


/**
 *	cipherUpdateRounds:
 *
 *	Encrypts/Decrypts exactly one full block a specified number of rounds.
 *	Only used in the Intermediate Value Known Answer Test.	
 *
 *	Returns:
 *		TRUE - on success
 *		BAD_CIPHER_STATE - cipher in bad state (e.g., not initialized)
 */
int cipherUpdateRounds(cipherInstance *cipher,
	keyInstance *key, BYTE *input, int inputLen, BYTE *outBuffer, int rounds)
{
	int j, t;
	word8 block[4][MAXBC];

	if (cipher == NULL ||
		key == NULL ||
		cipher->blockLen != key->blockLen) {
		return BAD_CIPHER_STATE;
	}

	for (j = 0; j < cipher->blockLen/32; j++) {
		for(t = 0; t < 4; t++)
			/* parse input stream into rectangular array */
			block[t][j] = input[4*j+t] & 0xFF;
	}
	switch (key->direction) {
	case DIR_ENCRYPT:
		rijndaelEncryptRound (block, key->keyLen, cipher->blockLen, 
				key->keySched, rounds);
	break;
		
	case DIR_DECRYPT:
		rijndaelDecryptRound (block, key->keyLen, cipher->blockLen, 
				key->keySched, rounds);
	break;
		
	default: return BAD_KEY_DIR;
	} 
	for (j = 0; j < cipher->blockLen/32; j++) {
		/* parse rectangular array into output ciphertext bytes */
		for(t = 0; t < 4; t++)
			outBuffer[4*j+t] = (BYTE) block[t][j];
	}
	
	return TRUE;
}
